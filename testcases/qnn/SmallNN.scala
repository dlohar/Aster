
import daisy.lang._
import Vector._

object SmallNN {

  def nn1(x: Vector): Vector = {
    require(lowerBounds(x, List(-10, -5)) &&
      upperBounds(x, List(10, 5)))

    val weights1 = Matrix(List(
      List(0.1, 0.3),
      List(0.2, 0.15)))

    val weights2 = Matrix(List(
      List(0.1, 0.2)))

    val bias1 = Vector(List(1.0, 2.0))
    val bias2 = Vector(List(0.5))

    val layer1 = relu(weights1 * x + bias1)
    val layer2 = relu(weights2 * layer1 + bias2)
    layer2

  } ensuring(res => res +/- 1e-3)
  // ensuring(res => res +/- 1e-5)

}
