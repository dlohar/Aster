package daisy
package solvers

import tools.Rational

import ilog.concert._
import ilog.cplex._

import tools.{Interval, Rational}

// cplex solver trait
trait CplexSolver {

  /* Creating the optimization model
    loPi: lower limit of the fractional bit length -- computed in NNQuantizationPhase
    hiPi: upper limit of the fractional bit length -- computed in NNQuantizationPhase
    layer: number of layers -- user provided
  */
  def createModel(loPi: Int, hiPi: Int, layer: Int) :
  (IloCplex, Array[IloIntVar], Array[IloIntVar], Array[IloNumVar], Array[IloNumVar]) = {
    val model: IloCplex = new IloCplex()

    // optimization variables that store the fractional bit lengths of dot and bias operations
    val piDot = Array.ofDim[IloIntVar](layer)
    val piBias = Array.ofDim[IloIntVar](layer)
    for (i <- 0 until layer) {
      val dotVar: IloIntVar = model.intVar(loPi, hiPi, s"dot_${i+1}") // Keeping the names
      val biasVar: IloIntVar = model.intVar(loPi, hiPi, s"bias_${i+1}") // Keeping the names
      piDot(i) = dotVar
      piBias(i) = biasVar
    }

    // optimization variables that store number of bits of dot and bias operations
    val twoPowerPiDot = Array.ofDim[IloNumVar](layer)
    val twoPowerPiBias = Array.ofDim[IloNumVar](layer)
    for (i <- 0 until layer) {
      val twoPowerdotVar: IloNumVar = model.numVar(Math.pow(2, -hiPi), Math.pow(2, -loPi), s"twoPowerPiDot_${i+1}") // Keeping the names
      val twoPowerbiasVar: IloNumVar = model.numVar(Math.pow(2, -hiPi), Math.pow(2, -loPi), s"twoPowerPiBias_${i+1}") // Keeping the names
      twoPowerPiDot(i) = twoPowerdotVar
      twoPowerPiBias(i) = twoPowerbiasVar
    }
    (model, piDot, piBias, twoPowerPiDot, twoPowerPiBias)
  }

/* cost definition:
    wordLengthWmax, wordLengthBias: list of ints -- computed in NNQunatizationPhase
    piDot, piBias: the number of fractional bits of dot and bias */
  def addObjectiveFunc(model: IloCplex,
    wordLengthWmax: List[Int],
    bias: List[List[Rational]],
    layer: Int,
    piDot: Array[IloIntVar],
    piBias: Array[IloIntVar],
    noOfInputs: Int): IloCplex = {

    var totalCost: IloLinearNumExpr = model.linearNumExpr() // total cost objective

    // variable to linearize the max function in the bias cost
    val costBias: Array[IloIntVar] = model.intVarArray(layer, 0, 100)

    // Computing the cost of activation
    val costAct = costActivationFunc(piBias)

    // Keeping the number of neurons at each layer
    val noOfNeurons: List[Int] = for {
      neurons <- bias
    } yield neurons.length

    for (i <- 0 until layer) {
      // Linearized the cost of adding the bias and adding them to the model
      model.addGe(costBias(i), piDot(i)).setName(s"lin_max_1_${i+1}")
      model.addGe(costBias(i), piBias(i)).setName(s"lin_max_2_${i+1}")

      // Adding the cost of the bias to the total cost
      totalCost.addTerm(costBias(i), 1.0)

      // Adding the activation cost to the total cost
      totalCost.addTerm(costAct(i), 1.0)
      var constMul = 0
      if (i != 0) {
        // Computing the cost of dot product at layer i != 0 and adding it to the total cost
        constMul = noOfNeurons(i) * noOfNeurons(i-1) * wordLengthWmax(i)

      } else {
        // Computing the cost of dot product at layer i = 0 and adding it to the total cost
        constMul = noOfNeurons(i) * noOfInputs * wordLengthWmax(i) // initial fractional bits are assigned
        // totalCost.setConstant(constMul)
      }
      totalCost.addTerm(piDot(i), constMul)

    }
    // Adding optimization objective
    model.addMinimize(totalCost)
    model
  }

  /* Adding error and range constraints
  maxWeights: max of weights
  initError: initial error -- user provided
  layer: # layers
  targetError: error bound -- user provided
  realRangeDot: real valued ranges of dot in each layer -- given by Daisy
  realRangeBias: real valued ranges of bias in each layer -- given by Daisy
  */
  def addConstraints(model: IloCplex,
    twoPowerPiDot: Array[IloNumVar],
    twoPowerPiBias: Array[IloNumVar],
    maxWeight: List[Rational],
    initError: Rational,
    layer: Int,
    targetError: Rational,
    realRangeDot: List[Interval],
    realRangeBias: List[Interval],
    intBitDot: List[Int],
    intBitBias: List[Int]): IloCplex = {
    // error expression
    var totalError: IloNumExpr = model.linearNumExpr()
    var propError: IloNumExpr = model.linearNumExpr()

    // initial error is assigned
    propError = model.sum(makeDouble(initError), propError)

    for (i <- 0 until layer) {
      // Compute the new error
      val newError = model.sum(twoPowerPiDot(i), twoPowerPiBias(i))
      // Compute the total error = new error + propagation error
      totalError = model.sum(newError, propError)

      // Add range constraints
      // Compute the maximum representable range after each operation
      val maxRepresentableRangeDot = Interval.+/-(Rational.powerTwo(intBitDot(i)-1)) // As we had sign bit added
      val maxRepresentableRangeBias = Interval.+/-(Rational.powerTwo(intBitBias(i)-1))

      // Compute the finite precision ranges after each operation
      val errorAfterDot = model.sum(twoPowerPiDot(i), propError)
      val finiteRangeLoDot = model.sum(makeDouble(realRangeDot(i).xlo), model.negative(errorAfterDot))
      val finiteRangeHiDot = model.sum(makeDouble(realRangeDot(i).xhi), errorAfterDot)

      val finiteRangeLoBias = model.sum(makeDouble(realRangeBias(i).xlo), model.negative(totalError))
      val finiteRangeHiBias = model.sum(makeDouble(realRangeBias(i).xhi), totalError)

      // finiteRange(op) is inside the maxRepresentableRange(op)
      model.addGe(finiteRangeLoDot, makeDouble(maxRepresentableRangeDot.xlo)).setName(s"range_dot_lo_${i+1}")
      model.addLe(finiteRangeHiDot, makeDouble(maxRepresentableRangeDot.xhi)).setName(s"range_dot_hi_${i+1}")

      model.addGe(finiteRangeLoBias, makeDouble(maxRepresentableRangeBias.xlo)).setName(s"range_bias_lo_${i+1}")
      model.addLe(finiteRangeHiBias, makeDouble(maxRepresentableRangeBias.xhi)).setName(s"range_bias_hi_${i+1}")

      // Compute new propagation error
      propError = model.prod(makeDouble(maxWeight(i)), totalError)

    }

    // Add the error constraint
    model.addLe(totalError, makeDouble(targetError)).setName("error")
    model
  }

  /* Adding linearization constraints
    loPi: lower limit of the fractional bit length -- computed in NNQuantizationPhase
    hiPi: upper limit of the fractional bit length -- computed in NNQuantizationPhase
  */
  def linearizeConstraints( model: IloCplex,
    piDot: Array[IloIntVar],
    piBias: Array[IloIntVar],
    twoPowerPiDot: Array[IloNumVar],
    twoPowerPiBias: Array[IloNumVar],
    loPi: Int, hiPi: Int, layer: Int): IloCplex = {

    val pi = (loPi to hiPi).toList // Bit length Interval for fractional bits
    val twoPowerPi = pi.map(a => math.pow(2, -a)) // Creating the 2^-pi list
    val len = pi.length

    for (i <- 0 until layer) {
      // Defining linearization constraints for dot and two power of dot
      val cdot: Array[IloNumVar] = model.numVarArray(len, 0, 1, IloNumVarType.Bool)
      var dotExpr: IloNumExpr = model.linearNumExpr()
      for (j <- 0 until len) {
        dotExpr = model.sum(cdot(j), dotExpr)
      }
      model.addEq(dotExpr, 1).setName(s"bin_dot_${i+1}") // sum of the cdots is 1.
      var dotTwoPowerExpr: IloNumExpr = model.linearNumExpr()
      var dotPiExpr: IloNumExpr = model.linearNumExpr()

      // Defining linearization constraints for bias and two power of bias
      val cbias: Array[IloNumVar] = model.numVarArray(len, 0, 1, IloNumVarType.Bool)
      var biasExpr: IloNumExpr = model.linearNumExpr()
      for (j <- 0 until len) {
        biasExpr = model.sum(cbias(j), biasExpr)
      }
      model.addEq(biasExpr, 1).setName(s"bin_bias_${i+1}")  // sum of the biases is 1.
      var biasTwoPowerExpr: IloNumExpr = model.linearNumExpr()
      var biasPiExpr: IloNumExpr = model.linearNumExpr()

      for (j <- 0 until len) {
        // Adding binary constraints for dot
        dotTwoPowerExpr = model.sum(model.prod(cdot(j), twoPowerPi(j)), dotTwoPowerExpr)
        dotPiExpr = model.sum(model.prod(cdot(j), pi(j)), dotPiExpr)
        // Adding binary constraints for bias
        biasTwoPowerExpr = model.sum(model.prod(cbias(j), twoPowerPi(j)), biasTwoPowerExpr)
        biasPiExpr = model.sum(model.prod(cbias(j), pi(j)), biasPiExpr)
      }

      model.addEq(dotTwoPowerExpr, twoPowerPiDot(i)).setName(s"err_dot_${i+1}") 
      model.addEq(dotPiExpr, piDot(i)).setName(s"pi_dot_${i+1}") 

      model.addEq(biasTwoPowerExpr, twoPowerPiBias(i)).setName(s"err_bias_${i+1}")
      model.addEq(biasPiExpr, piBias(i)).setName(s"pi_bias_${i+1}")
    }
    model
  }

  // solves the model
  def solve(model: IloCplex): Boolean = {
    val res = model.solve()
    res
  }

  // terminates the model
  def end(model: IloCplex) : Unit = {
    model.end()
  }

// Add activation cost
  private def costActivationFunc(piBias: Array[IloIntVar]) : Array[IloIntVar] = for {
      cost <- piBias
  } yield cost

  private def makeDouble(a: Rational) = a.toString.toDouble

}
