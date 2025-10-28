import daisy.lang._
import Real._

object VehicleDynamicControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.1137 <= x1 && x1 <= 0 && -1.5637 <= x2 && x2 <= 5.0442 && -7.4696 <= u && u <= 0.27936)
    val res = (1.0089000000) * x1 + (-0.0009241400) * x2 + (0.0011000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.1137 <= x1 && x1 <= 0 && -1.5637 <= x2 && x2 <= 5.0442 && -7.4696 <= u && u <= 0.27936)
    val res = (0.0261000000) * x1 + (1.0088000000) * x2 + (0.0916000000) * u
    res
  }

}