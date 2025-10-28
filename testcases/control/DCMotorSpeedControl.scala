import daisy.lang._
import Real._

object DCMotorSpeedControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.0062 <= x1 && x1 <= 1.5 && -0.1805 <= x2 && x2 <= 0 && -1.3138 <= u && u <= 0.0421)
    val res = (0.8187000000) * x1 + (0.0178000000) * x2 + (0.0003695900) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.0062 <= x1 && x1 <= 1.5 && -0.1805 <= x2 && x2 <= 0 && -1.3138 <= u && u <= 0.0421)
    val res = (-0.0003551500) * x1 + (0.9608000000) * x2 + (0.0392000000) * u
    res
  }

}