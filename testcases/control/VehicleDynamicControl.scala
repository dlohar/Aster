import daisy.lang._
import Real._

object VehicleDynamicControl {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.0114 <= x1 && x1 <= 0 && -0.1564 <= x2 && x2 <= 0.5044 && -0.747 <= u && u <= 0.0279)
    val res = (1.0089000000) * x1 + (-0.0009241400) * x2 + (0.0011000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.0114 <= x1 && x1 <= 0 && -0.1564 <= x2 && x2 <= 0.5044 && -0.747 <= u && u <= 0.0279)
    val res = (0.0261000000) * x1 + (1.0088000000) * x2 + (0.0916000000) * u
    res
  }

}