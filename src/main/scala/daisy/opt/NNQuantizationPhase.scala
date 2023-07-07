// TODO: rename the cplex_<arch>.jar to cplex.jar and edit build.sbt accordingly
package daisy
package opt

import java.io._

import scala.math.max
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Seq

import tools.FinitePrecision._
import lang.Identifiers._
import lang.Trees._
import lang.TreeOps._
import lang.Types._
import lang.Extractors.ArithOperator
import tools.{Interval, Rational, MPFRInterval, MPFRFloat}

object NNQuantizationPhase extends DaisyPhase with tools.RoundoffEvaluators with solvers.CplexSolver {

  override val name = "NN quantization phase"
  override val shortName = "nnQuantize"
  override val description = "determines a suitable mixed-precision type assignment for a NN"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("minLen", 10, "The minimum fractional bit length"),
    NumOption("maxLen", 32, "The maximum fractional bit length"),
    NumOption("initLen", 32, "The initial fractional bit length"),
    NumOption("layer", 2, "The number of layers in the classifier"),
    FlagOption("noInitError", "Assumes no initial error"),
    FlagOption("getInitLen", "Computes the initial length from the error target")


    /* These two are taken from the --precision flag
    NumOption("wlWmax", 32, "The maximum wordLength of weights") */
  )
  implicit val debugSection = DebugSectionOptimisation
  private var dimensions: Map[Identifier, List[Int]] = Map()

  var reporter: Reporter = null

  type TypeConfig = Map[Identifier, Precision]
  var weightMaxList = new ListBuffer[Rational]

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    val defaultPrecision = ctx.option[Precision]("precision")
    val hiPi = ctx.option[Long]("maxLen").toInt
    val loPi = ctx.option[Long]("minLen").toInt
    val initLen = ctx.option[Long]("initLen").toInt
    val nlayer = ctx.option[Long]("layer").toInt
    val rangeMethod = ctx.option[String]("rangeMethod")
    // val activErr = ctx.option[Double]("actError")
    val wordLengthWmax = List.fill(nlayer)(fixedBits(defaultPrecision))
    // val wordLengthWmax = List.fill(nlayer)(20)
    var precisionMap: Map[Identifier, Map[Identifier, Precision]] = Map()
    var resAbsoluteErrors: Map[Identifier, Rational] = Map()
    var realRange: Map[Identifier, Map[Identifier, Interval]] = Map()

    val newDef = transformConsideredFunctions(ctx, prg) { fnc => {
      reporter.info("analyzing fnc: " + fnc.id)
      // Getting unrolled body and original body
      val fncBody = fnc.body.get
      val origProg = ctx.originalFunctions(fnc.id)
      val origFncBody = origProg.body.get

      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)
      val targetError = ctx.specResultErrorBounds(fnc.id)
      val piIn = if(!ctx.hasFlag("getInitLen")) initLen
                 else ((log2(targetError)).abs + 5) // Increasing the initial input bit length by 5 to keep the inital error lower
      val initError = if(ctx.hasFlag("noInitError")) Rational.zero
                    else Rational.powerTwo(-piIn)
      if(initError > targetError) {
        reporter.fatalError("Initial error is already greater than target error! Not possible to optimize. Exiting...")
      }
      // Getting the input parameters and all variables of the function
      val inputParams: Seq[Identifier] = fnc.params.map(_.id)
      val noOfInputs: Int = inputParams.length
      val allVarOfFunc = allVariablesOf(fncBody)

      val flatWeightBuffer = new ListBuffer[List[Rational]]
      var biasBuffer = new ListBuffer[List[Rational]]
      val numOfColInWeight = new ListBuffer[Rational] // Keeps the number of additions for dot
      var count = 0

      // Getting weights and biases from the original function body
      val weight = getWeight(origFncBody).toList
      biasBuffer = biasBuffer ++ getBias(origFncBody).toList
      for (i <- weight) {
        flatWeightBuffer += i.flatten
        numOfColInWeight += i(0).size
      }

      val numOfTerms: List[Rational] = numOfColInWeight.toList
      val flattenWeights: List[List[Rational]] = flatWeightBuffer.toList
      // Computing max weights
      val maxWeight: List[Rational] = weightMaxList.toList

      val biases: List[List[Rational]] = biasBuffer.toList

      // Generating intermediate real ranges
      reporter.info("Generating Intermediate Ranges ... ")
      val realRangeMap = (rangeMethod: @unchecked) match {
        case "interval" => analysis.NNRangeAnalysisPhase.evalNNRange(origProg, fncBody, inputRanges)
        case "intervalMPFR" => (analysis.NNRangeAnalysisPhase.evalNNRangeMPFR(origProg,
          fncBody, inputRanges.mapValues(MPFRInterval(_)))).mapValues(Interval(_))
      }
      // val (resRange, intermRealRange) = evalRange[Interval](fncBody, inputRanges, Interval.apply)
      realRange += (fnc.id -> realRangeMap)

      val (realRangeDot, realRangeBias, intBitDot, intBitBias) = getRealRanges(fncBody, realRangeMap)

      // initializing the model
      reporter.info("Generating Constraints ...")
      val (initModel, piDot, piBias, twoPowerPiDot, twoPowerPiBias) = createModel(loPi, hiPi, nlayer)
      // adding linearization constraints
      var model = linearizeConstraints(initModel, piDot, piBias, twoPowerPiDot, twoPowerPiBias, loPi, hiPi, nlayer)
      // adding error and range constraints
      model = addConstraints(model, twoPowerPiDot, twoPowerPiBias, maxWeight, initError, nlayer, targetError,
        realRangeDot, realRangeBias, intBitDot, intBitBias)
      // adding cost function
      model = addObjectiveFunc(model, wordLengthWmax, biases, nlayer, piDot, piBias, noOfInputs)
      /* solving the model
      val res = solve(model) */
      // exporting the model
      model.exportModel("model.lp")
      end(model)

      reporter.info("Running SCIP...")
      val (fracBitLenMap, resErrorMap) = generateResults()

      val resError: Rational = getTotalError(resErrorMap, nlayer, maxWeight, initError)
      resAbsoluteErrors = resAbsoluteErrors + (fnc.id -> resError)

      val (typeConfig, retPrec) = quantizedPrecisionSOPC(fncBody, numOfTerms, fracBitLenMap, realRangeMap, nlayer, piIn)
      val daisy_cost_func = areaCostFixedPoint(fncBody, typeConfig)
      reporter.info("According to Daisy's cost: " + daisy_cost_func)
      // final step: apply quantized precision
      val updatedBody = applyQuantizedPrecision(fncBody, typeConfig)(retPrec)
      val updatedParams = fnc.params.map(valDef =>
        ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

      val newReturnType = fnc.returnType match {
        case TupleType(_) =>
          TupleType(ctx.resultTupleIds(fnc.id).map(id => FinitePrecisionType(typeConfig(id))))
        case _ => FinitePrecisionType(retPrec)
      }
      precisionMap = precisionMap + (fnc.id -> typeConfig)
      fnc.copy(returnType = newReturnType, params = updatedParams, body = Some(updatedBody))
    }}
    // update rangeMap with trees with casts
    /* val newIntermediateRanges = newDef.map(fnc => {
      var newRanges = Map[(Expr, Seq[Expr]), Interval]()
      var currRanges = realRange
      //ctx.intermediateRanges(fnc.id)

      postTraversal(e => {

        newRanges = newRanges + ((e, Seq()) -> currRanges((MixedPrecisionOptimizationPhase.exprWithoutCasts(e), Seq())))

      })(fnc.body.get)

      (fnc.id -> newRanges)
      }).toMap */

    (ctx.copy(resultAbsoluteErrors = resAbsoluteErrors,
      intermediateRealRanges = realRange,
      // intermediateRanges = newIntermediateRanges,
      specInputPrecisions = precisionMap), Program(prg.id, newDef))
  }

  /* Returns ceil(log2(value)) */
  def log2 (value: Rational): Int = {
    return (java.lang.Math.log(value.toDouble)/java.lang.Math.log(2)).ceil.toInt
  }

  /* Returns the set of weights */
  private def getWeight(e: Expr): Seq[List[List[Rational]]] = e match {
    case Let(id, MatrixLiteral(value), body) =>
    // Getting the additions for the next layer neurons then getting the max
    val totalWeights = value.map(_.reduce(_+_))
    val absWeights = totalWeights.map(Rational.abs(_))
    val maxOfAddWeights = absWeights.reduce(Rational.max(_,_))
    weightMaxList += maxOfAddWeights
    Seq(value) ++ getWeight(body)
    case _ => Seq()
  }

  /* Returns the set of biases */
  private def getBias(e: Expr): Seq[List[Rational]] = e match {
    case Let(id, VectorLiteral(value), next) => Seq(value) ++ getBias(next)
    case Let(_, _, body) => getBias(body)
    case _ => Seq()
  }

  // Gives the number of bits of Fixed Precision Type
  def fixedBits(prec: Precision): Int = (prec: @unchecked) match {
    case FixedPrecision(a) => a
  }

  /* Returns the total error */
  private def getTotalError(errorMap: Map[String, Rational],
    layer: Int,
    maxWeight: List[Rational],
    initError: Rational): Rational = {
    var totalError: Rational = Rational.zero
    var propError: Rational = initError
    for(i <- 0 until layer) {
      /* Keeping this code just to make sure that we don't get runtime error here!
      Changed considering I am not getting error = 0 anymore from the SCIP solver! */
      val dotError = errorMap.getOrElse(s"twoPowerPiDot_${i+1}", null)
      val biasError = errorMap.getOrElse(s"twoPowerPiBias_${i+1}", null)
      if (dotError == null || biasError == null) {
        reporter.fatalError("Error can not be 0. SCIP did not optimize correctly! Exiting...")
      }
      val newError = dotError + biasError
      // Compute the total error = new error + propagation error
      totalError = newError + propError
      // Compute new propagation error
      propError = maxWeight(i) * totalError

    }
    reporter.info("Total Error: " + totalError)
    totalError
  }

  /* Generating results from SCIP Solver */
  private def generateResults() : (Map[String, Int], Map[String, Rational]) = {
    val timeOut = 9000
    // val timeOut = 18000
    var bitLenMap = collection.mutable.Map[String, Int]()
    val timestamp: Long = System.currentTimeMillis / 1000
    var errorMap = collection.mutable.Map[String, Rational]()
    val start = System.currentTimeMillis


    val f: Future[Unit] = Future {
      // Run soplex with the model until timeout
      val res = Runtime.getRuntime().exec("/var/tmp/qnn/scipoptsuite-7.0.3/scip_build/bin/scip -f model.lp -s scip.set")
      // val res = Runtime.getRuntime().exec("../../scipOptSuite/bin/scip -f model.lp -s scip.set")
      // Read back the results
      val stdInput =  new BufferedReader(new InputStreamReader(res.getInputStream))
      var readResLine = stdInput.readLine()
      var resLineStr = readResLine.toString
      var resFlag = false

      // Recover the dot and bias bit lengths
      while(!(resLineStr contains "Statistics")) {
        println(resLineStr)

        if (resLineStr.startsWith("objective value:")) {
          resFlag = true
          val objective = resLineStr.split("\\s+")
          reporter.info("Objective: " + objective(2))
        }
        readResLine = stdInput.readLine()
        resLineStr = readResLine.toString

        if (resFlag) {
          val varArr = resLineStr.split("\\s+")
          if (varArr(0) contains "twoPowerPi") {
            errorMap += (varArr(0) -> Rational.fromString(varArr(1)))
          }
          if ((varArr(0) contains "dot") || (varArr(0) contains "bias")) {
            bitLenMap += (varArr(0) -> varArr(1).toInt)

          }
        }

      }
      res.getInputStream().close()
    }

    try {
      Await.result(f, timeOut.second)
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        reporter.warning(s"SCIP has timed out after $timeOut seconds.");
      case e: java.io.IOException =>
        reporter.warning(e.getMessage())
      case e: Exception =>
        reporter.warning("Something went wrong. Here's the stack trace:");
        e.printStackTrace
    }
    val stop = System.currentTimeMillis
    val time = stop-start
    reporter.info("Solver time: " + time + "ms\n")
    if (bitLenMap.isEmpty)
      reporter.fatalError("The solution is infeasible! Exiting...")

    (bitLenMap.toMap, errorMap.toMap)
  }

  /* Generate the precision map for all variables */
  private def quantizedPrecisionSOPC(fnc: Expr,
    numOfTerms: List[Rational],
    bitLenMap: Map[String, Int],
    realRangeMap: Map[Identifier, Interval],
    nlayer: Int,
    fracInput: Int): (TypeConfig, Precision) = {
    var quantizedPrecision: Map[Identifier, Precision] = Map()
    val guardedBit = new ListBuffer[Int]
    var resultPrec: Int = 0
    // val fracInput = (bitLenMap.filter((x) => x._1 contains (s"_${nlayer}"))).valuesIterator.max
    var layer = 0
    var noOfDot = 0
    for (i <- 0 until nlayer) {
      guardedBit += bitLenMap(s"dot_${i+1}") + log2(numOfTerms(i)) + 1 // fracBit(dot(i)) + ceil(log2 n)
    }

    def eval(e: Expr): Unit = (e: @unchecked) match {
      case Let(id, RealLiteral(_), body) => eval(body)

      case Let(id, Times(Variable(lhs), Variable(rhs)), body) =>
        if (rhs.toString.startsWith(s"layer${layer+1}")) layer += 1
        val intBitInput = FixedPrecision.unsignedIntegerBitsNeeded(Interval.maxAbs(realRangeMap(rhs)))
        val intBitWeight = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeMap(lhs)))
        val precWeight = guardedBit(layer) + intBitInput + intBitWeight
        val precTmp = guardedBit(layer) + FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeMap(id)))
        quantizedPrecision += (id -> FixedPrecision(precTmp))
        quantizedPrecision += (lhs -> FixedPrecision(precWeight))
        if (!(quantizedPrecision contains rhs)) {
          val intWordLen = fracInput + intBitInput + 1
          quantizedPrecision += (rhs -> FixedPrecision(intWordLen))
        }
        eval(body)

      case Let(id, Plus(Variable(lhs), Variable(rhs)), body) =>
        if (id.toString.startsWith("_tmp")) {
          // val layerRange = (realRangeMap.filter{case (k,v) => k.toString contains (s"layer${layer+1}_dot_${noOfDot}")}).values.toList
          val tmpIntBit = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeMap(id)))
          val tmpPrec = tmpIntBit + guardedBit(layer)
          quantizedPrecision += (id -> FixedPrecision(tmpPrec))

        } else {
          val intBit = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeMap(id)))
          val fracBit = if (id.toString contains "dot") bitLenMap(s"dot_${layer+1}")
            else bitLenMap(s"bias_${layer+1}")
          quantizedPrecision += (id -> FixedPrecision(intBit + fracBit))
          if (rhs.toString.startsWith("bias")) {
            val intBitBias = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeMap(rhs)))
            quantizedPrecision += (rhs -> FixedPrecision(intBitBias + fracBit))
          }
        }
        eval(body)

      case Let(id, Relu(Variable(t)), body) =>
        quantizedPrecision += (id -> quantizedPrecision(t))
        eval(body)

      case Let(id, Linear(Variable(t)), body) =>
        quantizedPrecision += (id -> quantizedPrecision(t))
        eval(body)

      case Let(id, Variable(t), body) if (id.toString.startsWith("#res")) =>
        quantizedPrecision += (id -> quantizedPrecision(t))
        resultPrec = resultPrec.max(fixedBits(quantizedPrecision(id)))
        eval(body)

      case Variable(t) => None
    }
    eval(fnc)

    (quantizedPrecision, FixedPrecision(resultPrec))
  }

  /* Returns the real valued ranges after each operation.*/
  private def getRealRanges(fnc: Expr, realRangeMap: Map[Identifier, Interval]):
    (List[Interval], List[Interval], List[Int], List[Int]) = {
    val dotRange = new ListBuffer[Interval]()
    val biasRange = new ListBuffer[Interval]()
    val intBitDot = new ListBuffer[Int]()
    val intBitBias = new ListBuffer[Int]()

    var layer = 0
    var realRangeDot: Interval = Interval.apply(0)
    var realRangeBias: Interval = Interval.apply(0)


    def eval(e: Expr): Unit = (e: @unchecked) match {
      case Let(id, RealLiteral(_), body) => eval(body)
      case Let(id, Times(Variable(lhs), Variable(rhs)), body) =>
        if (rhs.toString.startsWith(s"layer${layer+1}")) {
          dotRange += realRangeDot
          intBitDot += FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeDot)) // We do not add 1 later for sign
          biasRange += realRangeBias
          intBitBias += FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeBias)) // We do not add 1 later for sign
          layer += 1
        }
        eval(body)
      case Let(id, Plus(Variable(lhs), Variable(rhs)), body) =>
        if (id.toString contains s"layer${layer+1}") {
          if  (id.toString contains "_0") {
            if (id.toString contains "dot") realRangeDot = realRangeMap(id)
            else realRangeBias = realRangeMap(id)
          } else {
            if (id.toString contains "dot") realRangeDot = realRangeDot.union(realRangeMap(id))
            else realRangeBias = realRangeBias.union(realRangeMap(id))
          }
        }
        eval(body)
      case Let(id, Relu(Variable(t)), body) => eval(body)
      case Let(id, Linear(Variable(t)), body) => eval(body)
      case Let(id, Variable(t), body) => eval(body)
      case _ => None
    }
    eval(fnc)
    // Adding the last layer
    dotRange += realRangeDot
    intBitDot += FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeDot)) // We do not add 1 later for sign
    biasRange += realRangeBias
    intBitBias += FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRangeBias)) // We do not add 1 later for sign

    (dotRange.toList, biasRange.toList, intBitDot.toList, intBitBias.toList)
  }

  def applyQuantizedPrecision(expr: Expr, typeConfig: Map[Identifier, Precision])(implicit returnPrec: Precision): Expr = {

    def recurse(e: Expr): Expr = (e: @unchecked) match {

      case x @ Variable(id) =>
        Variable(id.changeType(FinitePrecisionType(typeConfig(id))))

      case Let(id, x @ RealLiteral(r), body) =>
        val idPrec = typeConfig(id)
        val newValue = FinitePrecisionLiteral(r, idPrec, x.stringValue)

        Let(id.changeType(FinitePrecisionType(idPrec)), newValue, recurse(body))

      // tuple result
      case Let(id, x @ Variable(t), body) if (id.toString.startsWith("#res")) =>
        val varPrec = typeConfig(t)
        val newValue = Variable(t.changeType(FinitePrecisionType(varPrec)))

        Let(id.changeType(FinitePrecisionType(varPrec)), newValue, recurse(body))

      case Let(id, x @ ArithOperator(Seq(t @ Variable(tId)), recons), body) =>
        val idPrec = typeConfig(id)
        val tPrec = typeConfig(tId)

        val updatedT = recurse(t)
        val opPrec = getUpperBound(tPrec, idPrec)

        // We only need to introduce an upcast for the inner operation or a downcast for the outer operation.
        // It can't happen that both are necessary, since tPrec < opPrec ==> idPrec = opPrec and
        // idPrec < opPrec ==> tPrec = opPrec.
        val withCasts: Expr =  if (tPrec < opPrec) { // need to introduce upcast for operation
          recons(Seq(Cast(updatedT, FinitePrecisionType(idPrec))))
        } else if (idPrec < opPrec) { // need to downcast for assignment
          Cast(recons(Seq(updatedT)), FinitePrecisionType(idPrec))
        } else {
          recons(Seq(recurse(t)))
        }

        Let(id.changeType(FinitePrecisionType(idPrec)), withCasts, recurse(body))


      case Let(id, x @ ArithOperator(Seq(y @ Variable(lhs), z @ Variable(rhs)), recons), body) =>
        var left = recurse(y)
        var right = recurse(z)

        val idPrec = typeConfig(id)
        val lPrec = typeConfig(lhs)
        val rPrec = typeConfig(rhs)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // forced upcasts
        /* if (lPrec != opPrec) {
          left = Cast(left, FinitePrecisionType(opPrec))
        }
        if (rPrec != opPrec) {
          right = Cast(right, FinitePrecisionType(opPrec))
        }*/

        var tmp = recons(Seq(left, right))

        if (idPrec < opPrec) { //need to downcast
          tmp = Cast(tmp, FinitePrecisionType(idPrec))
        }
        Let(id.changeType(FinitePrecisionType(idPrec)), tmp, recurse(body))

      case x @ ArithOperator(Seq(t @ Variable(_)), recons) =>
        recons(Seq(recurse(t)))

      case x @ ArithOperator(Seq(y @ Variable(_), z @ Variable(_)), recons) =>
        val lPrec = typeConfig(y.id)
        val rPrec = typeConfig(z.id)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), returnPrec)

        var left = recurse(y)
        var right = recurse(z)

        // forced upcasts
        if (lPrec != opPrec) {
          left = Cast(left, FinitePrecisionType(opPrec))
        }
        if (rPrec != opPrec) {
          right = Cast(right, FinitePrecisionType(opPrec))
        }

        recons(Seq(left, right))
    }
    recurse(expr)
  }

  // assumes casts cost nothing
  def areaCostFixedPoint(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    def eval(e: Expr): Int = e match {
      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body) 

      // unary minus : bitlength(input) because it is similar to the adder
      case Let(id, UMinus(Variable(t)), body) =>
        fixedBits(typeConfig(t)) + eval(body)

      // plus: max(bitlength(lhs), bitlength(rhs), bitlength(result))
      case Let(id, Plus(Variable(lhs), Variable(rhs)), body) =>
        max(fixedBits(typeConfig(lhs)), max(fixedBits(typeConfig(rhs)), fixedBits(typeConfig(id)))) + 
          eval(body)

      //minus: max(bitlength(lhs), bitlength(rhs), bitlength(result))
      case Let(id, Minus(Variable(lhs), Variable(rhs)), body) =>
        max(fixedBits(typeConfig(lhs)), max(fixedBits(typeConfig(rhs)), fixedBits(typeConfig(id)))) + 
          eval(body)

      // mult: bitlength(lhs) * bitlength(rhs)
      case Let(id, Times(Variable(lhs), Variable(rhs)), body) =>
        fixedBits(typeConfig(lhs)) * fixedBits(typeConfig(rhs)) + 
          eval(body)

      // division : bitlength(output) * max(bitlength(lhs), bitlength(rhs)) for a digit-recurrent division, which is the most reasonable one
      case Let(id, Division(Variable(lhs), Variable(rhs)), body) =>
        fixedBits(typeConfig(id)) * max(fixedBits(typeConfig(lhs)), fixedBits(typeConfig(rhs)))

      // tuple result, costs nothing
      case Let(id, Variable(t), body) if (id.toString.startsWith("#res")) =>
        0

      // assuming this is implemented as an if-statement, roughly
      case Let(id, Relu(Variable(t)), body) =>
        fixedBits(typeConfig(t)) + eval(body)

        // assuming this is implemented as an if-statement, roughly
      case Let(id, Linear(Variable(t)), body) =>
        fixedBits(typeConfig(t)) + eval(body)

      case UMinus(Variable(t)) =>
        fixedBits(typeConfig(t))

      case Plus(Variable(lhs), Variable(rhs)) =>
        max(fixedBits(typeConfig(lhs)), fixedBits(typeConfig(rhs)))

      case Minus(Variable(lhs), Variable(rhs)) =>
        max(fixedBits(typeConfig(lhs)), fixedBits(typeConfig(rhs)))

      case Times(Variable(lhs), Variable(rhs)) =>
        fixedBits(typeConfig(lhs)) * fixedBits(typeConfig(rhs))

      case Division(Variable(lhs), Variable(rhs)) =>
        fixedBits(typeConfig(lhs)) * fixedBits(typeConfig(rhs))


    }
    Rational(eval(expr))
  }


}
