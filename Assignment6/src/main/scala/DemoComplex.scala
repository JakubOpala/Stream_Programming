object DemoComplex {
  import Complex._
  import scala.language.implicitConversions
  implicit def doubletoComplex(x: Double) = new Complex(x)

  def main(args: Array[String]): Unit = {

    val x = new Complex(2, 3)
    val y = Complex(5, 7)
    val z = Complex(2)

    val multiplus = abs(2 * x + y * z)
    val divminus = conj(2 / x - y / z)

    println(s"x=$x y=$y z=$z")
    println("abs(2 * x + y * z) = " + multiplus)
    println("conj(2 / x - y / z) = " + divminus)
  }


}
