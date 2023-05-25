object DecodeH74 extends App{

  def decode(s: String): String = {

    var bits: Array[Int] = s.toArray.map(_.asDigit)
    val p1 = (bits(0) + bits(1) + bits(3)) % 2
    val p2 = (bits(0) + bits(2) + bits(3)) % 2
    val p3 = (bits(1) + bits(2) + bits(3)) % 2
    s + p1.toString + p2.toString + p3.toString

    if (p1 != bits(4) && p2 != bits(5) && p3 != bits(6)){
      bits(3) = 1 - bits(3)
      println("Error in 4th bit")
    } else if (p1 != bits(4) && p2 != bits(5)) {
      bits(0) =  1-bits(0)
      println("Error in 1st bit")
    } else if (p1 != bits(4) && p3 != bits(6)) {
      bits(1) =  1-bits(1)
      println("Error in 2nd bit")
    } else if (p2 != bits(5) && p3 != bits(6)) {
      bits(2) =  1-bits(2)
      println("Error in 3rd bit")
    } else if (p1 != bits(4)) {
      bits(4) = 1 - bits(4)
      println("Error in 1st parity bit")
    } else if (p2 != bits(5)) {
      bits(5) = 1 - bits(5)
      println("Error in 2nd parity bit")
    } else if (p3 != bits(6)) {
      bits(6) = 1 - bits(6)
      println("Error in 3rd parity bit")
    }

    bits.mkString("")
  }

  val code: String = "1010010"
  val decode: String = decode(code) // Error in 4th bit

  println("Code: " + code) // Code: 1010010
  println("Decode: " + decode) // Decode: 1011010

}
