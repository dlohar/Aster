
import daisy.lang._
import Vector._

object TestNN {

  def nn1(x: Vector): Vector = {
    require(lowerBounds(x, List(-10, -5)) &&
      upperBounds(x, List(10, 5)))

    val weights1 = Matrix(List(
      List(1.2, 2.3),
      List(3.4, 4.5),
      List(4.5, 5.6)))

    val weights2 = Matrix(List(
      List(1.1, 2.2, 3.3),
      List(2.2, 3.3, 4.4),
      List(3.3, 4.4, 5.5)))

    val bias1 = Vector(List(1.0, 2.0, 3.0))
    val bias2 = Vector(List(0.5, 0.6, 0.7))

    val layer1 = relu(weights1 * x + bias1)
    val layer2 = relu(weights2 * layer1 + bias2)
    layer2

  } ensuring(res => res +/- 1e-3)
  // ensuring(res => res +/- 1e-5)

}
