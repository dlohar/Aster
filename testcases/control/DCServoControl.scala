import daisy.lang._
import Real._

object DCServoControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-21.1940 <= x1 && x1 <= 35.3234 && 1 <= x2 && x2 <= 10.2507 && -16.014 <= u && u <= 26.69)
    val res = (0.9976000000) * x1 + (0.0000000000) * x2 + (0.4495000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-21.1940 <= x1 && x1 <= 35.3234 && 1 <= x2 && x2 <= 10.2507 && -16.014 <= u && u <= 26.69)
    val res = (0.0200000000) * x1 + (1.0000000000) * x2 + (0.0045000000) * u
    res
  }

}