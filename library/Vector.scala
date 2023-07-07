
package daisy.lang


@ignore
object Vector {
  def relu(x: Vector): Vector = ???

  def tanh(x: Vector): Vector = ???

  def linear(x: Vector): Vector = ???

  def lowerBounds(x: Vector, bounds: List[Double]): Boolean = ???

  def upperBounds(x: Vector, bounds: List[Double]): Boolean = ???
}

@ignore
case class Vector(data: List[Double]) {
  def +(v: Vector): Vector = ???

  // Uncertainty on this vector
  def +/-(x: Real): Boolean = ???
}

@ignore
case class Matrix(data: List[List[Double]]) {
  def *(v: Vector): Vector = ???
}
