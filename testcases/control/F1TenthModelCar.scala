import daisy.lang._
import Real._

object F1TenthModelCar {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(0.0821 <= x1 && x1 <= 0.5 && -0.4586 <= x2 && x2 <= 0.2293 && -0.4763 <= u && u <= 0.4248)
    val res = (1.0000000000) * x1 + (0.1300000000) * x2 + (0.0256000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(0.0821 <= x1 && x1 <= 0.5 && -0.4586 <= x2 && x2 <= 0.2293 && -0.4763 <= u && u <= 0.4248)
    val res = (0.0000000000) * x1 + (1.0000000000) * x2 + (0.3937000000) * u
    res
  }

}