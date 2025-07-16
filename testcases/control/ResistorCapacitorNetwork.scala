import daisy.lang._
import Real._

object ResistorCapacitorNetwork {

  def next_x1(x1: Real, x2: Real, u: Real): Real = {
    require(-0.004 <= x1 && x1 <= 5 && -0.2304 <= x2 && x2 <= 0.0187 && -9.1902 <= u && u <= 0.0348)
    val res = (0.8870000000) * x1 + (0.0187000000) * x2 + (0.0943000000) * u
    res
  }

  def next_x2(x1: Real, x2: Real, u: Real): Real = {
    require(-0.004 <= x1 && x1 <= 5 && -0.2304 <= x2 && x2 <= 0.0187 && -9.1902 <= u && u <= 0.0348)
    val res = (0.0037000000) * x1 + (0.9861000000) * x2 + (0.0101000000) * u
    res
  }

}