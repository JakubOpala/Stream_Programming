object Hamming_dist {

  def hamming_distance(a: Int, b: Int): Int = {
    Integer.bitCount(a ^ b)
  }

  def main(args: Array[String]): Unit = {
    val a = 1729
    val b = a * a
    println("Hamming distance between 1729 and 1729^2: " + hamming_distance(a, b)) // 10
    println("1729 as binary: " + a.toBinaryString) // 1729 as binary: 11011000001
    println("1729^2 as binary: " + b.toBinaryString) // 1729^2 as binary: 1011011001110110000001
  }

}
