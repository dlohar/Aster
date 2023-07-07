
package daisy
package analysis

import scala.collection.immutable.HashMap

import lang.Trees._
import lang.Identifiers._
import tools.{Interval, Rational, MPFRInterval, MPFRFloat}


object NNRangeAnalysisPhase extends DaisyPhase with tools.RangeEvaluators {
  override val name = "NN range analysis"
  override val shortName = "NN range analysis"
  override val description = "Computes ranges for NN"

  implicit val debugSection = DebugSectionAnalysis

  // This function is just used for testing, call
  // NNRangeAnalysisPhase.evalNNRange directly
  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val res = analyzeConsideredFunctions(ctx, prg){ fnc =>
      val fncBody = fnc.body.get

      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val origFnc = ctx.originalFunctions(fnc.id)

      var start = System.currentTimeMillis()
      val rangeMapNN = evalNNRange(origFnc, fncBody, inputRanges)
      var end = System.currentTimeMillis()
      println("time for NN: " + (end - start))

      // baseline: copied from NNQuantPhase
      start = System.currentTimeMillis()
      val (resRange, intermRealRange) = evalRange[Interval](fncBody, inputRanges, Interval.apply)

      val realRange = intermRealRange
      val realRangeMap: Map[Identifier, Interval] = lang.TreeOps.allVariablesOf(fncBody).map({
        id => (id -> intermRealRange(Variable(id), emptyPath))
      }).toMap
      end = System.currentTimeMillis()
      println("time for baseline: " + (end - start))

      //sanity check: compare NN ranges to baseline:
      for(k <- realRangeMap.keys) {
        if (!rangeMapNN(k).includes(realRangeMap(k))) {
          println("!!! unsound for " + k)
        }
        //println(s"$k  baseline-NNrange: ${realRangeMap(k)} -  ${rangeMapNN(k)}")
      }

      realRangeMap
    }

    (ctx, prg)
  }

  /**
   * Computes intermediate ranges based on the original NN representation
   * @param origFnc: original NN representation of the function with explicit dot product
   * @param unrolledExpr: unrolled body of NN
   * @param inputRanges: initial ranges (of unrolled body) as stored in the context
   * @returns ranges for all variables in the unrolled code
   */
  def evalNNRange(origFnc: FunDef, unrolledExpr: Expr, inputRanges: Map[Identifier, Interval]): Map[Identifier, Interval] = {

    val nnExpr = origFnc.body.get

    // FIXME: we assume that there is only 1 argument...
    // to get the overall input range, union all individual input ranges
    val origParamID = origFnc.params.map(_.id).head
    val inputRange = inputRanges.values.fold(inputRanges.values.head)((acc, x) => acc.union(x))
    val initValMap = Map(origParamID -> inputRange)

    val exprVars = lang.TreeOps.allVariablesOf(unrolledExpr)

    // map from variable name (as String) to variable with proper ID, so that we match those vars in the unrolled code
    val varName2ID: Map[String, Identifier] = exprVars.map(x => (x.toString -> x)).toMap

    // this is the info that we need to compute, initialize with the (unrolled) inputs
    var intrmRanges = inputRanges

    // dim stores the relevant dimensions of the matrices and vectors
    def eval(e: Expr, valMap: Map[Identifier, Interval], dim: Map[Identifier, Int], tmpCount: Int): Map[Identifier, Interval] = e match {
      // case Matrix: over-approximate all values into one range
      //      and add to intermRanges following naming pattern
      case Let(id, m @ MatrixLiteral(value), next) =>
        val min = value.map(row => row.min).min
        val max = value.map(row => row.max).max
        val matrixRange = Interval(min, max)

        for (i <- 0 until value.length) {
          for (j <- 0 until value.head.length) {
            intrmRanges += (varName2ID(s"${id.toString}_${i}_${j}") -> matrixRange)
          }
        }

        eval(next, valMap + (id -> matrixRange), dim + (id -> value.head.length), tmpCount)

      // val bias1 = Vector(List(1.0, 2.0))
      // case Vector: over-approximate all values into one range
      case Let(id, v @ VectorLiteral(value), next) =>
        val vectorRange = Interval(value.min, value.max)

        for (i <- 0 until value.length) {
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> vectorRange)
        }

        eval(next, valMap + (id -> vectorRange), dim + (id -> value.length), tmpCount)

      // val layer1 = relu(weights1 * x + bias1) = relu(m * x + b)
      // val layer2 = relu(weights2 * layer1 + bias2)
      case Let(id, Relu(
        VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>

        // compute the ranges
        // w_1*x_1 + w_2*x_2 + w_3*x_3 ... = dim * w_i * x_i
        val mDim = dim(mId)
        val dotRange = valMap(mId) * valMap(xId) * mDim

        // dot_i + b_i
        val biasRange = dotRange + valMap(bId)

        val reluRange = Interval(Rational.max(0, biasRange.xlo), Rational.max(0, biasRange.xhi))

        // assign variables in unrolled code, except for tmp variables
        for (i <- 0 until dim(bId)) {  // corresponding to dot product
          intrmRanges += (varName2ID(s"${id.toString}_dot_${i}") -> dotRange)
        }
        for (i <- 0 until dim(bId)) { //corresponding to bias and relu computation
          intrmRanges += (varName2ID(s"${id.toString}_bias_${i}") -> biasRange)
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> reluRange)
        }

        eval(next, valMap + (id -> reluRange), dim, tmpCount)

      case Let(id, Linear(
         VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>

        // compute the ranges
        // w_1*x_1 + w_2*x_2 + w_3*x_3 ... = dim * w_i * x_i
        val mDim = dim(mId)
        val dotRange = valMap(mId) * valMap(xId) * mDim

        // dot_i + b_i
        val biasRange = dotRange + valMap(bId)

        val linearRange = biasRange

        // assign variables in unrolled code, except for tmp variables
        for (i <- 0 until dim(bId)) {  // corresponding to dot product
          intrmRanges += (varName2ID(s"${id.toString}_dot_${i}") -> dotRange)
        }
        for (i <- 0 until dim(bId)) { //corresponding to bias and linear computation
          intrmRanges += (varName2ID(s"${id.toString}_bias_${i}") -> biasRange)
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> linearRange)
        }

        eval(next, valMap + (id -> linearRange), dim, tmpCount)

      case Variable(id) =>
        valMap
    }

    // first compute the ranges on the NN
    val nnRanges = eval(nnExpr, initValMap, new HashMap(), 0)

    // assigns only the temporary variables
    def assignUnrolled(e: Expr, valMap: Map[Identifier, Interval]): Map[Identifier, Interval] = e match {

      // let (weights1_0_1 := 0.2
      // let (bias1_0 := 1.0)
      case Let(id, l@RealLiteral(_), next) =>
        assignUnrolled(next, valMap)

      // let (_tmp := (weights1_0_0 * x_0))
      case Let(id, Times(Variable(lhsId), Variable(rhsId)), next) =>
        val resRange = valMap(lhsId) * valMap(rhsId)
        assignUnrolled(next, valMap + (id -> resRange))

      // let (layer1_dot_0 := (_tmp + _tmp1))
      case Let(id, Plus(Variable(lhsId), Variable(rhsId)), next) if (lhsId.toString.startsWith("_tmp")) =>
        val resRange = valMap(lhsId) + valMap(rhsId)
        assignUnrolled(next, valMap + (id -> resRange))

      // or let (layer1_bias_0 := (layer1_dot_0 + bias1_0))
      case Let(id, Plus(Variable(lhsId), Variable(biasId)), next) =>
        assignUnrolled(next, valMap)

      case Let(id, Relu(Variable(rId)), next) =>
        assignUnrolled(next, valMap)

      case Let(id, Linear(Variable(rId)), next) =>
        assignUnrolled(next, valMap)

      // #res variables
      case Let(id, Variable(lId), next) =>
        assignUnrolled(next, valMap + (id -> valMap(lId)))

      case Variable(id) =>
        valMap
    }

    // then assign the ranges to the _tmp_i variables
    val res = assignUnrolled(unrolledExpr, intrmRanges)
    res
  }

  def evalNNRangeMPFR(origFnc: FunDef, unrolledExpr: Expr, inputRanges: Map[Identifier, MPFRInterval]): Map[Identifier, MPFRInterval] = {

    val nnExpr = origFnc.body.get

    // FIXME: we assume that there is only 1 argument...
    // to get the overall input range, union all individual input ranges
    val origParamID = origFnc.params.map(_.id).head
    val inputRange = inputRanges.values.fold(inputRanges.values.head)((acc, x) => acc.union(x))
    val initValMap = Map(origParamID -> inputRange)

    val exprVars = lang.TreeOps.allVariablesOf(unrolledExpr)

    // map from variable name (as String) to variable with proper ID, so that we match those vars in the unrolled code
    val varName2ID: Map[String, Identifier] = exprVars.map(x => (x.toString -> x)).toMap

    // this is the info that we need to compute, initialize with the (unrolled) inputs
    var intrmRanges = inputRanges

    // dim stores the relevant dimensions of the matrices and vectors
    def eval(e: Expr, valMap: Map[Identifier, MPFRInterval], dim: Map[Identifier, Int], tmpCount: Int): Map[Identifier, MPFRInterval] = e match {
      // case Matrix: over-approximate all values into one range
      //      and add to intermRanges following naming pattern
      case Let(id, m @ MatrixLiteral(value), next) =>
        val min = value.map(row => row.min).min
        val max = value.map(row => row.max).max
        val matrixRange = MPFRInterval(Interval(min, max))

        for (i <- 0 until value.length) {
          for (j <- 0 until value.head.length) {
            intrmRanges += (varName2ID(s"${id.toString}_${i}_${j}") -> matrixRange)
          }
        }

        eval(next, valMap + (id -> matrixRange), dim + (id -> value.head.length), tmpCount)

      // val bias1 = Vector(List(1.0, 2.0))
      // case Vector: over-approximate all values into one range
      case Let(id, v @ VectorLiteral(value), next) =>
        val vectorRange = MPFRInterval(Interval(value.min, value.max))

        for (i <- 0 until value.length) {
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> vectorRange)
        }

        eval(next, valMap + (id -> vectorRange), dim + (id -> value.length), tmpCount)

      // val layer1 = relu(weights1 * x + bias1) = relu(m * x + b)
      // val layer2 = relu(weights2 * layer1 + bias2)
      case Let(id, Relu(
        VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>

        // compute the ranges
        // w_1*x_1 + w_2*x_2 + w_3*x_3 ... = dim * w_i * x_i
        val mDim = dim(mId)
        val dotRange = valMap(mId) * valMap(xId) * mDim

        // dot_i + b_i
        val biasRange = dotRange + valMap(bId)

        val reluRange = MPFRInterval(MPFRFloat.max(MPFRFloat.zero, biasRange.xlo), MPFRFloat.max(MPFRFloat.zero, biasRange.xhi))

        // assign variables in unrolled code, except for tmp variables
        for (i <- 0 until dim(bId)) {  // corresponding to dot product
          intrmRanges += (varName2ID(s"${id.toString}_dot_${i}") -> dotRange)
        }
        for (i <- 0 until dim(bId)) { //corresponding to bias and relu computation
          intrmRanges += (varName2ID(s"${id.toString}_bias_${i}") -> biasRange)
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> reluRange)
        }

        eval(next, valMap + (id -> reluRange), dim, tmpCount)

      case Let(id, Linear(
         VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>

        // compute the ranges
        // w_1*x_1 + w_2*x_2 + w_3*x_3 ... = dim * w_i * x_i
        val mDim = dim(mId)
        val dotRange = valMap(mId) * valMap(xId) * mDim

        // dot_i + b_i
        val biasRange = dotRange + valMap(bId)

        val linearRange = biasRange

        // assign variables in unrolled code, except for tmp variables
        for (i <- 0 until dim(bId)) {  // corresponding to dot product
          intrmRanges += (varName2ID(s"${id.toString}_dot_${i}") -> dotRange)
        }
        for (i <- 0 until dim(bId)) { //corresponding to bias and linear computation
          intrmRanges += (varName2ID(s"${id.toString}_bias_${i}") -> biasRange)
          intrmRanges += (varName2ID(s"${id.toString}_${i}") -> linearRange)
        }

        eval(next, valMap + (id -> linearRange), dim, tmpCount)

      case Variable(id) =>
        valMap
    }

    // first compute the ranges on the NN
    val nnRanges = eval(nnExpr, initValMap, new HashMap(), 0)

    // assigns only the temporary variables
    def assignUnrolled(e: Expr, valMap: Map[Identifier, MPFRInterval]): Map[Identifier, MPFRInterval] = e match {

      // let (weights1_0_1 := 0.2
      // let (bias1_0 := 1.0)
      case Let(id, l@RealLiteral(_), next) =>
        assignUnrolled(next, valMap)

      // let (_tmp := (weights1_0_0 * x_0))
      case Let(id, Times(Variable(lhsId), Variable(rhsId)), next) =>
        val resRange = valMap(lhsId) * valMap(rhsId)
        assignUnrolled(next, valMap + (id -> resRange))

      // let (layer1_dot_0 := (_tmp + _tmp1))
      case Let(id, Plus(Variable(lhsId), Variable(rhsId)), next) if (lhsId.toString.startsWith("_tmp")) =>
        val resRange = valMap(lhsId) + valMap(rhsId)
        assignUnrolled(next, valMap + (id -> resRange))

      // or let (layer1_bias_0 := (layer1_dot_0 + bias1_0))
      case Let(id, Plus(Variable(lhsId), Variable(biasId)), next) =>
        assignUnrolled(next, valMap)

      case Let(id, Relu(Variable(rId)), next) =>
        assignUnrolled(next, valMap)

      case Let(id, Linear(Variable(rId)), next) =>
        assignUnrolled(next, valMap)

      // #res variables
      case Let(id, Variable(lId), next) =>
        assignUnrolled(next, valMap + (id -> valMap(lId)))

      case Variable(id) =>
        valMap
    }

    // then assign the ranges to the _tmp_i variables
    val res = assignUnrolled(unrolledExpr, intrmRanges)
    res
  }

}
