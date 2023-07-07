
import daisy.lang._
import Vector._

object LyapunovNN {

  def nn1(x: Vector): Vector = {
    require(lowerBounds(x, List(-6, -6)) && 
      upperBounds(x, List(6, 6)))

    // Input is a 2-element vector 

    // Matrix of size 6 x 2 
    val weights1 = Matrix(List(
      List(-0.06348759, 0.019289402), 
      List(0.031379573, -0.0024394097), 
      List(1.0211772, 0.01997059),
      List(-0.6363012, -0.3944512),
      List(-0.035791364, -0.53223646),
      List(0.62558943, 0.09921459)
      ))    

    // Matrix of size 1 x 6 
    val weights2 = Matrix(List(
      List(-0.5230915, 1.1081733, -0.80846876, 0.40946653, -0.08949807, 0.91387314)))

    // Vector of size 6 x 1
    val bias1 = Vector(List(-0.8746956, 1.1860801, 1.1548865, -1.2558243, 1.5483683, -0.77857643))
    // Vector of size 1 x 1 
    val bias2 = Vector(List(0.5307131))

    val layer1 = relu(weights1 * x + bias1)
    val layer2 = relu(weights2 * layer1 + bias2)
    layer2
  } ensuring(res => res +/- 1e-3)
  // ensuring(res => res +/- 1e-5)
}
