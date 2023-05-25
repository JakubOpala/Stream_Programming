import scala.math.pow

object Hamming {

  def hamming_weight(n: Int): Int = {
    val n_binary = n.toBinaryString.toCharArray()

    val res: Int = n_binary.foldLeft(0) {
      (acc, xi) =>
        if (xi == '1') {
          acc + 1
        }
        else{
          acc
        }
    }
    res
  }

  def main(args: Array[String]): Unit = {

    val n = pow(1729,2).toInt
    val w = hamming_weight(n)
    val n_binary = n.toBinaryString

    //Binary rep of 1729^2: 1011011001110110000001
    //Hamming weight of 1729^2 = 11

    println("Binary rep of 1729^2: " + n_binary)
    println("Hamming weight of 1729^2 = " + w)

  }

}
