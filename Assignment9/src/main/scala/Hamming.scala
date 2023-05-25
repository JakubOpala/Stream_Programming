import scala.math.pow

object Hamming {

  def hamming_dist(x: Int, y: Int): Int = {
    val x_binary = x.toBinaryString.toCharArray()
    val y_binary = y.toBinaryString.toCharArray()

    val x_len = x_binary.length
    val y_len = y_binary.length

    val x1 = Array.fill(0.max(y_len-x_len))('0') ++ x_binary
    println(x1.mkString(""))
    val y1 = Array.fill(0.max(x_len-y_len))('0') ++ y_binary
    println(y1.mkString(""))

    val res = (x1 zip y1).count{ case (x, y) => x != y}

    res
  }

  def main(args: Array[String]): Unit = {

    val x = 1729
    val y = pow(1729,2).toInt

    //val w = hamming_dist(x,y)

    val x_binary = "0b" + x.toBinaryString
    val y_binary = "0b" + y.toBinaryString

    val w = x ^ y

    //Binary rep of 1729: 11011000001
    //Binary rep of 1729^2: 1011011001110110000001
    //Hamming distance between 1729 and 1729^2 = 6

    println("Binary rep of 1729: " + x_binary)
    println("Binary rep of 1729^2: " + y_binary)
    println("Hamming distance between 1729 and 1729^2 = " + w)

  }

}
