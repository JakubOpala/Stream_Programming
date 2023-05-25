object Hamming74 extends App{

  def H74(s: String): String = {
    val bits: Array[Int] = s.toArray.map(_.asDigit)
    val p1 = (bits(0) + bits(1) + bits(3)) % 2
    val p2 = (bits(0) + bits(2) + bits(3)) % 2
    val p3 = (bits(1) + bits(2) + bits(3)) % 2
    s + p1.toString + p2.toString + p3.toString
  }

  val transmission: String = "1011"
  val encode: String = H74(transmission)
  println("Transmission: " + transmission) // Transmission: 1011
  println("Encoding: " + encode) // Encoding: 1011010

}
