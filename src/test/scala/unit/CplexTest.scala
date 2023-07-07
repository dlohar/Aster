package unit

import daisy.solvers.CplexSolver
import daisy.tools.{Interval, Rational}



class CplexTest extends UnitSuite with CplexSolver {

  ignore("Simple Cplex Optimization test") {
    val loPi = 16// 18
    val hiPi = 64
    val piIn = 32
    val layer = 2
    // val actError = 1.0
    val weights = List(List(Rational.fromReal(0.1), Rational.fromReal(0.2), Rational.fromReal(0.2),
        Rational.fromReal(0.15)), List(Rational.fromReal(0.1), Rational.fromReal(0.2)))
    val bias = List(List(Rational.fromReal(1.0), Rational.fromReal(2.0)), List(Rational.fromReal(0.5)))
    val maxWeight = List(Rational.fromReal(0.2), Rational.fromReal(0.2))
    val initError = Rational.fromReal(0.0)
    val eps = Rational.fromReal(1e-18)
    val wordLengthWmax = List(64,64)
    val intBitRealRange = List(Interval(-20.5, 20.5), Interval(-20.5, 20.5))
    val intBitFiniteRange = List(Interval(-21.0, 21.0), Interval(-21.0, 21.0))
    val intBitDot = List(4, 5)
    val intBitBias = List(5, 6)
    val noOfInputs = 2
    val (initModel, piDot, piBias, twoPowerPiDot, twoPowerPiBias) = createModel(loPi, hiPi, layer)
    var model = linearizeConstraints(initModel, piDot, piBias, twoPowerPiDot, twoPowerPiBias, loPi, hiPi, layer)
    model = addConstraints(model, twoPowerPiDot, twoPowerPiBias, maxWeight, initError, layer, eps, intBitRealRange, intBitFiniteRange, intBitDot, intBitBias)
    model = addObjectiveFunc(model, wordLengthWmax, bias, layer, piDot, piBias, noOfInputs)
    model.exportModel("model.lp")

    /* val res = solve(model)
    if (res) {
      val cost = model.getObjValue()
      println(s"Cost :" + cost)
      for (i <- 0 until layer) {
        println(s"piDot["+i + "] :"+ Math.ceil(model.getValue(piDot(i))))
        println(s"piBias["+i + "] :"+ Math.ceil(model.getValue(piBias(i))))
        println(s"twoPowerPiDot["+i + "] :" + model.getValue(twoPowerPiDot(i)))
        println(s"twoPowerPiBias["+i + "] :" + model.getValue(twoPowerPiBias(i)))
      }
    }
    // model.exportModel("model.lp")*/
    end(model)
  }
}




