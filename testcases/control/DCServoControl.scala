import daisy.lang._
import Real._

object DCServoControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.2914 <= x1 && x1 <= 0.3886 && 0 <= x2 && x2 <= 0.0429 && -0.243 <= u && u <= 0.3241)
    val res = (0.9976000000) * x1 + (0.0000000000) * x2 + (0.4495000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.2914 <= x1 && x1 <= 0.3886 && 0 <= x2 && x2 <= 0.0429 && -0.243 <= u && u <= 0.3241)
    val res = (0.0200000000) * x1 + (1.0000000000) * x2 + (0.0045000000) * u
    res
  }

}