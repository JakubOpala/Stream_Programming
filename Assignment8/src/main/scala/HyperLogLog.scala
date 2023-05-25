import scala.annotation.tailrec
import scala.util.Random.nextInt
import scala.io.Source
import scala.util.hashing.MurmurHash3

object HyperLogLog {

  def exact_distinct(words: Array[String]): Int = {
    words.distinct.length
  }

  def hyperloglog(words: Array[String]): Array[Int] = {

    val hashes = words.map(MurmurHash3.stringHash(_))
    val n = hashes.length

    val m = 1024
    val buckets = Array.fill(m)(0)

    @tailrec
    def fill_buckets(index: Int = 0): Unit = {
      if (index < n) {

        val num: Int = hashes(index)
        val modulo: Int = mod(num, m)
        val first1 = Integer.numberOfLeadingZeros(num) + 1

        if (first1 > buckets(modulo)) buckets(modulo) = first1

        fill_buckets(index + 1)
      }
    }

    fill_buckets()

    val Z = buckets.map(math.pow(2, _))
    val Z_arithmetic = Z.sum.toInt
    val Z_harmonic = m * m / (Z.map(x => 1 / x).sum).toInt

    Array(Z_arithmetic, Z_harmonic)

  }

  def mod(x: Int, y: Int): Int = ((x % y) + y) % y

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      // We take contents of file as one big string and split it to get array of strings - words
      val file = args(0)
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString.toLowerCase().split("\\W+")

      val ex = exact_distinct(words)
      val hll = hyperloglog(words)
      // We use distinct method to eliminate multiple-times occuring words


      fileSource.close

      """
        |For “Alice’s adventures in wonderland”:
        |exact number of distinct words: 2580
        |hyperloglog with arithmetic average: 40020
        |hyperloglog with harmonic average: 3615
        |
        |For the Bible:
        |exact number of distinct words: 12473
        |hyperloglog with arithmetic average: 180928
        |hyperloglog with harmonic average: 18396
        |""".stripMargin

      println("exact number of distinct words: " + ex)
      println("hyperloglog with arithmetic average: " + hll(0))
      println("hyperloglog with harmonic average: " + hll(1))
    }
    else println("no file")
  }


}
