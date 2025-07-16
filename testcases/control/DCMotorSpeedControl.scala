import daisy.lang._
import Real._

object DCMotorSpeedControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.00011714 <= x1 && x1 <= 0.5 && -0.0061 <= x2 && x2 <= 0 && -0.0374 <= u && u <= 0.00010505)
    val res = (0.8187000000) * x1 + (0.0178000000) * x2 + (0.0003695900) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.00011714 <= x1 && x1 <= 0.5 && -0.0061 <= x2 && x2 <= 0 && -0.0374 <= u && u <= 0.00010505)
    val res = (-0.0003551500) * x1 + (0.9608000000) * x2 + (0.0392000000) * u
    res
  }

}