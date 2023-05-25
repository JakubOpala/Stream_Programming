object StoCount2 {

  import scala.util.Random._
  import scala.annotation.tailrec

  def stoch_count(m: Int, n: Int): Array[Double] = {


    val stream = (1 to n).map(_ => nextInt())
    val buckets = Array.fill(m)(0)

    @tailrec
    def fill_buckets(index: Int = 0): Unit = {
      if (index < n) {

        val num: Int = stream(index)
        val modulo: Int = mod(num, m)
        val first1 = Integer.numberOfLeadingZeros(num) + 1

        if (first1 > buckets(modulo)) buckets(modulo) = first1

        fill_buckets(index + 1)
      }
    }

    fill_buckets()

    /*
    val stream = Array.fill(n)(nextInt())

    val buckets = stream.foldLeft(Array.fill(m)(0)) {
      (acc, xi) =>
        val bucket_num = mod(xi, m)
        val first1 = Integer.numberOfLeadingZeros(xi) + 1
        if (first1 > acc(bucket_num)) {
          acc(bucket_num) = first1
        }
        acc
    }
     */

    val dist_num: Double = stream.toSet.size

    val Z = buckets.map(math.pow(2, _))
    val Z_arithmetic = Z.sum
    val Z_harmonic = m * m / (Z.map(x => 1 / x).sum)

    Array(Z_arithmetic, Z_harmonic, dist_num)
  }

  def mod(x: Int, y: Int): Int = ((x % y) + y) % y

  def main(args: Array[String]): Unit = {

    val m = 256
    val n = math.pow(2, 24).toInt
    val res = stoch_count(m, n)
    println("Approx of distinct elements in stream by stochastic counting using arithmetic mean: " + res(0).toInt)
    println("Approx of distinct elements in stream by stochastic counting using harmonic mean: " + res(1).toInt)
    println("Exact number of distinct elements from stream: " + res(2).toInt)

    // Example of program results for stream of 2^24 pseudorandom integers:
    // Approx of distinct elements in stream by stochastic counting using arithmetic mean: 129613824
    // Approx of distinct elements in stream by stochastic counting using harmonic mean:   20792186
    // Exact number of distinct elements from stream:                                      16744164
    // For not very big sets harmonic mean gives an accuracy in range of about 25%, while using arithmetic mean
    // evaluation is almost 8 times bigger than the correct number.

  }

}
