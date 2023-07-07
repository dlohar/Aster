// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer

import utils.CodePrinter
import lang.Trees._
import tools.FinitePrecision
import FinitePrecision._
import tools.FinitePrecision._
import lang.Types._
import lang.TreeOps._

import lang.Extractors.ArithOperator
import tools.{Interval, Rational}
import lang.Identifiers.Identifier
import lang.TreeOps.{getLastExpression}


object NNCodeGenerationPhase extends DaisyPhase {
  override val name = "NN Code Generation"
  override val shortName = "nncodegen"
  override val description = "Generates (executable) NN code."
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "lang",
      Set("C", "Scala", "FPCore"),
      "Scala",
      "Language for which to generate code"),
    FlagOption(
      "genMain",
      "Whether to generate a main method to run the code."),
    FlagOption(
      "apfixed",
      "Print C code for Xilinx with ap_fixed data type"),
    FlagOption(
      "codegenNNOriginal",
      "(temporary flag) to generate code for the original program with loops"
    )
  )

  implicit val debugSection = DebugSectionBackend

  var reporter: Reporter = null

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecisions = ctx.uniformPrecisions

    reporter = ctx.reporter
    val fixedPrecision = ctx.fixedPoint
    val apfixedFormat = ctx.hasFlag("apfixed")

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>

      // if the function returns tuples, we need to re-tuple them
      val _body = fnc.returnType match {
        case TupleType(_) => reTuple(fnc.body.get, ctx.resultTupleIds(fnc.id))
        case _ => fnc.body.get
      }

      // if (fixedPrecision && apfixedFormat) {
        // number of bits given in precision flag
      val b = (ctx.option[Precision]("precision"): @unchecked) match {
        case FixedPrecision(a) => a
      }
      val newBody = toAPFixedCode(_body, b, ctx.intermediateRealRanges(fnc.id))

      val fncParams = fnc.params.map({
        case ValDef(id) =>
          val realRange = ctx.intermediateRealRanges(fnc.id)(id)
          // (Variable(id), emptyPath)
          // +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id), emptyPath)

          val parTotal = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            // case RealType => (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
          }

          val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRange))
          ValDef(id.changeType(APFixedType(parTotal, intBits)))
      })

      val retType = fnc.returnType match {
        case FinitePrecisionType(FixedPrecision(a)) =>
          val realRange = ctx.resultRealRanges(fnc.id)
          // +/- ctx.resultAbsoluteErrors(fnc.id)
          val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRange))
          APFixedType(a, intBits)

        case TupleType(resTypes) =>
          getLastExpression(_body) match {
            case Tuple(args) =>
              TupleType(args.zip(resTypes).map({

                // check that the result is actually a Variable
                case (Variable(resId), retType) =>
                  val totalBits = retType match {
                    case FinitePrecisionType(FixedPrecision(a)) => a // types already changed
                    // case RealType => (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
                  }

                  val realRange = ctx.intermediateRealRanges(fnc.id)(resId)
                  // (Variable(resId), emptyPath)
                  // +/- ctx.intermediateAbsErrors(fnc.id)(Variable(resId), emptyPath)
                  val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRange))
                  APFixedType(totalBits, intBits)
              }))
          }
        }
        val precision = (ctx.option[Precision]("precision"): @unchecked) match {
          case FixedPrecision(a) => a
        }
        val costFunction = computeCost(fncParams, newBody, retType, ctx.originalFunctions(fnc.id), precision)
          reporter.info("Area cost: " + costFunction)

        if (ctx.hasFlag("codegenNNOriginal")) {
          assignQNNOriginalPrecision(fncParams, newBody, retType, ctx.originalFunctions(fnc.id), precision)
        } else {
          fnc.copy(
            params = fncParams,
            body = Some(newBody),
            returnType = retType)
        }
    }

    val newProgram = Program(prg.id, newDefs)

    val lang = "apfixed"
    // else ctx.option[String]("lang")

    writeFile(newProgram, lang, ctx)

    (ctx, newProgram)
  }

  def fixedBits(prec: Precision): Int = (prec: @unchecked) match {
    case FixedPrecision(a) => a
  }

  private def reTuple(e: Expr, resIds: Seq[Identifier]): Expr = e match {
    // found the first inserted result ID
    case Let(id, _, _) if (resIds.contains(id)) =>
      // replace with just the tuple
      Tuple(reconstructTuple(e))

    case Let(id, value, body) => Let(id, value, reTuple(body, resIds))

    case _ => e

  }

  private def reconstructTuple(e: Expr): Seq[Expr] = e match {
    // last one, i.e. base case
    case Let(_, Variable(id), Variable(_)) =>
      Seq(Variable(id))

    case Let(_, Variable(id), body) =>
      reconstructTuple(body) :+ Variable(id)
  }

  private def writeFile(prg: Program, lang: String, ctx: Context): Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter

    val filename = lang match {
      case "apfixed" => System.getProperty("user.dir")+"/output/" + prg.id + ".cpp"
      case _ =>
      System.getProperty("user.dir")+"/output/" + prg.id + CodePrinter.suffix(lang)
    }
    ctx.codegenOutput.append(prg.id)
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    CodePrinter(prg, ctx, lang, out)
  }

  /*
   * Generates code for the Vivado HLS hardware synthesis tool with the ap_fixed data type.
   * Expects code to be already in SSA form.
   */
  def toAPFixedCode(expr: Expr,
    totalBits: Int,
    intermRanges: Map[Identifier, Interval]): Expr = {

    @inline
    def getIntegerBits(id: Identifier): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val realRange = intermRanges(id)
      // actualRange = actualRange +/- intermAbsErrors(e, path)
      FixedPrecision.integerBitsNeeded(Interval.maxAbs(realRange))
    }

    def _toFPCodeNew(e: Expr): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x

      case x @ FinitePrecisionLiteral(r,prec,s)  => x

      case Let(id, x @ ArithOperator(Seq(t: Expr), recons), body) =>
        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        val idType = APFixedType(bits, getIntegerBits(id))
        Let(id.changeType(idType), recons(Seq(_toFPCodeNew(t))), _toFPCodeNew(body))

      case Let(id, x @ ArithOperator(Seq(lhs: Expr, rhs: Expr), recons), body) =>

        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        val rhsFP = _toFPCodeNew(rhs)
        val lhsFP = _toFPCodeNew(lhs)
        val idType = APFixedType(bits, getIntegerBits(id))

        Let(id.changeType(idType), recons(Seq(lhsFP, rhsFP)), _toFPCodeNew(body))

      case x @ Cast(ex,typ) =>
        val bits = typ match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        ex match {
          case Variable(id) => Cast(ex, APFixedType(bits,getIntegerBits(id)))
          case UMinus(Variable(id)) => Cast(ex, APFixedType(bits,getIntegerBits(id)))
          // case _ => Cast(_toFPCodeNew(ex), APFixedType(bits,getIntegerBits(ex, path)))
        }
      case Let(id, Cast(ex, typ), body) =>

        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }

        val idType = APFixedType(bits, getIntegerBits(id))
        ex match {
          case Variable(x) => Let(id.changeType(idType), Cast(ex, APFixedType(bits, getIntegerBits(x))), _toFPCodeNew(body))
          // case UMinus(Variable(x)) => Cast(ex, APFixedType(bits,getIntegerBits(x)))

          case _ => Let(id.changeType(idType), Cast(ex, APFixedType(bits, getIntegerBits(id))), _toFPCodeNew(body))
        }
      case Let(id, value, body) =>
        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        val idType = APFixedType(bits, getIntegerBits(id))
        Let(id.changeType(idType), _toFPCodeNew(value),
          _toFPCodeNew(body))

      case x @ Tuple(_) => x

    }
    _toFPCodeNew(expr)
  }

  // assign fixed-point precision types to original neural network functions
  private def assignQNNOriginalPrecision(fncParams: Seq[ValDef], newBody: Expr,
    retType: TypeTree, originalFnc: FunDef, prec: Int): FunDef = {

    def unifyAPFixed(aps: Set[TypeTree]): APFixedType = {
      // the final/overall precision is determined as <totalbits, intBits>
      // - totalbits = max-fractional-bits + max-integer-bits (to satisfy rounding error)
      // - intBits = max-integer-bits (to satisfy no overflow)

      val (maxIntBits: Int, maxFpBits: Int) = aps.foldLeft((-1, -1)){
        case ((currMaxInt, currMaxFp), APFixedType(totalBits, intBits)) =>
          ( math.max(currMaxInt, intBits), math.max(currMaxFp, totalBits - intBits) )
      }
      // assumes that overall bitlength is limited to 64 bits
      APFixedType(math.min(prec, maxIntBits + maxFpBits), maxIntBits)
    }

    def _toFPCode(e: Expr, typeMap: Map[Identifier, APFixedType]): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x

      // TODO: revisit with mixed-precision
      case x @ Cast(ex,typ) => ???

      case Let(id, matrx @ MatrixLiteral(value),  body) =>
        Let(id.changeType(typeMap(id)), matrx, _toFPCode(body, typeMap))

      case Let(id, vectr @ VectorLiteral(value), body) =>
        Let(id.changeType(typeMap(id)), vectr, _toFPCode(body, typeMap))

      case Let(id, relu @ Relu(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>

        val idType = typeMap(id)

        // we really want to annotate the operations, but will try to annotate
        // the IDs here as a proxy, even though the precision assigned here
        // may be different from the precision assigned to the variables before
        Let(id.changeType(idType), Relu(VectorPlus(
          LinearMap(Variable(mId.changeType(idType)), Variable(xId.changeType(idType))),
            Variable(bId.changeType(idType)))), _toFPCode(body, typeMap))

      case Let(id, linear @ Linear(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>

        val idType = typeMap(id)

        // we really want to annotate the operations, but will try to annotate
        // the IDs here as a proxy, even though the precision assigned here
        // may be different from the precision assigned to the variables before
        Let(id.changeType(idType), Linear(VectorPlus(
          LinearMap(Variable(mId.changeType(idType)), Variable(xId.changeType(idType))),
            Variable(bId.changeType(idType)))), _toFPCode(body, typeMap))
    }

    val originalBody = originalFnc.body.get

    val newVars: Set[Identifier] = allLetVariablesOf(newBody)

    val originalIDs: Set[Identifier] = allLetVariablesOf(originalBody)

    val typeMap = originalIDs.map(origID => {
      // we identify matching IDs using the variable names
      // since we have control over the naming, this should be fine
      val relevantVars: Set[Identifier] = newVars.filter(i => i.toString.startsWith(origID.toString + "_"))
      val newPrecs = relevantVars.map(_.getType)
      val unifiedPrecision = unifyAPFixed(newPrecs)
      (origID -> unifiedPrecision)
    }).toMap

    val apFixedBody = _toFPCode(originalBody, typeMap)

    val apFixedReturnType = retType match {
      case TupleType(args) => TupleType(Seq.fill(args.size)(unifyAPFixed(args.toSet)))
    }
    originalFnc.copy(
      params = fncParams,
      body = Some(apFixedBody),
      returnType = apFixedReturnType
    )
  }

  // Returns the set of all variables occuring in let expressions.
  // We cannot use the allVariablesOf function in TreeOps, because that does
  // retain the needed type information (some occurrences have the right type, others don't)
  private def allLetVariablesOf(expr: Expr): Set[Identifier] = {
    lang.TreeOps.fold[Set[Identifier]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case Let(i, _, _) => subvs + i
          case _ => subvs
        }
    }(expr)
  }
  def computeCost(fncParams: Seq[ValDef], newBody: Expr,
    retType: TypeTree, originalFnc: FunDef, prec: Int): Rational = {

    def unifyAPFixed(aps: Set[TypeTree]): APFixedType = {
      // the final/overall precision is determined as <totalbits, intBits>
      // - totalbits = max-fractional-bits + max-integer-bits (to satisfy rounding error)
      // - intBits = max-integer-bits (to satisfy no overflow)

      val (maxIntBits: Int, maxFpBits: Int) = aps.foldLeft((-1, -1)){
        case ((currMaxInt, currMaxFp), APFixedType(totalBits, intBits)) =>
          ( math.max(currMaxInt, intBits), math.max(currMaxFp, totalBits - intBits) )
      }
      // assumes that overall bitlength is limited to 64 bits
      APFixedType(math.min(prec, maxIntBits + maxFpBits), maxIntBits)
    }

    val originalBody = originalFnc.body.get

    val newVars: Set[Identifier] = allLetVariablesOf(newBody)

    val originalIDs: Set[Identifier] = allLetVariablesOf(originalBody)

    val typeMap = originalIDs.map(origID => {
      // we identify matching IDs using the variable names
      // since we have control over the naming, this should be fine
      val relevantVars: Set[Identifier] = newVars.filter(i => i.toString.startsWith(origID.toString + "_"))
      val newPrecs = relevantVars.map(_.getType)
      val unifiedPrecision = unifyAPFixed(newPrecs)
      (origID -> unifiedPrecision)
    }).toMap

    val noOfInputs = fncParams.size
    var noOfNeurons = new ListBuffer[Int]
    var layer = 0

      // Gives the number of bits of Fixed Precision Type
    def intBits(apfixedFormat: APFixedType): Int = (apfixedFormat: @unchecked) match {
      case APFixedType(a, b) => b
    }
    def totalBits(apfixedFormat: APFixedType): Int = (apfixedFormat: @unchecked) match {
      case APFixedType(a, b) => a
    }

    def eval(e: Expr): Rational = e match {
      case Let(id, matrx @ MatrixLiteral(value),  body) =>
        eval(body)
      case Let(id, vectr @ VectorLiteral(value), body) =>
        noOfNeurons += value.size
        eval(body)

      case Let(id, relu @ Relu(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>
        var dotCost = Rational.zero
        val wordLengthW = totalBits(typeMap(mId))
        val dot = totalBits(typeMap(id)) - intBits(typeMap(id))
        if (xId.toString.startsWith("x")) {
          dotCost = noOfNeurons(layer) * noOfInputs * wordLengthW * dot
        } else {
          dotCost = noOfNeurons(layer) * noOfNeurons(layer-1) * wordLengthW * dot
        }
        layer += 1
        val bias = totalBits(typeMap(id)) - intBits(typeMap(id))
        val biasCost = Rational.max(bias, dot)
        val actCost = biasCost
        dotCost + biasCost + actCost + eval(body)

      case Let(id, linear @ Linear(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>
        var dotCost = Rational.zero
        val wordLengthW = totalBits(typeMap(mId))
        val dot = totalBits(typeMap(id)) - intBits(typeMap(id))
        if (xId.toString.startsWith("x")) {
          dotCost = noOfNeurons(layer) * noOfInputs * wordLengthW * dot
        } else {
          dotCost = noOfNeurons(layer) * noOfNeurons(layer-1) * wordLengthW * dot
        }
        layer += 1
        val bias = totalBits(typeMap(id)) - intBits(typeMap(id))
        val biasCost = Rational.max(bias, dot)
        val actCost = biasCost
        dotCost + biasCost + actCost + eval(body)
      case _ => Rational.zero
    }
    eval(originalBody)
  }

}
