import scala.annotation.tailrec
import scala.util.Random.nextInt
import scala.math._

object HLL {

  def exact_distinct(words: Array[String]): Int = {
    words.distinct.length
  }

  def hyperloglog(n: Int, runs: Int, run: Int, est_sum: Double = 0): Double = {
    if (run < 1) {
      (est_sum / runs).toInt
    }
    else{
      val b = 10
      val m = pow(2,b).toInt
      val buckets = Array.fill(m)(0)

      @tailrec
      def fill_buckets(index: Int = 0): Unit = {
        if (index < n) {



          val num: Int = nextInt()
          val j: Int = num >>> 32-b//mod(num, m)
          val w: Int = num << b
          val first1 = Integer.numberOfLeadingZeros(w) + 1

          if (first1 > buckets(j)) buckets(j) = first1

          fill_buckets(index + 1)
        }
      }

      fill_buckets()

      val Z = buckets.map(math.pow(2, _).toInt)
      val Z_arithmetic = Z.sum.toInt
      //val Z_harmonic = m * m / (Z.map(x => 1 / x).sum).toInt
      val Z_harmonic = m * m / (buckets.map(1 /pow(2, _)).sum)

      val alpha_m = 0.7213 / (1 + 1.079 / m)
      var estimation = Z_harmonic  * alpha_m

      val v = buckets.count(_ == 0)

      if (estimation <= m * 2.5) {
        if (v != 0) estimation = m * log(m.toDouble / v)
        println("Small range correction")
      }
      else if (estimation > pow(2, 32) / 30) {
        estimation = -pow(2, 32) * log(1 - estimation / pow(2, 32))
        println("Large range correction")
      }
      else println("Intermediate range — no correction")

      hyperloglog(n, runs, run-1, est_sum+estimation)
    }
  }

  def mod(x: Int, y: Int): Int = ((x % y) + y) % y

  def main(args: Array[String]): Unit = {

    val p = 28
    val n = pow(2,p).toInt
    val k: BigInt = pow(2,32).toInt
    val res: BigInt = k - hyperloglog(n, 10, 10).toInt
    val E: BigInt = k * ((k-1)/k)^n

    //println("exact number of distinct words: " + ex)
    //println("hyperloglog with arithmetic average: " + res(0))
    println("hyperloglog estimation " + res)
    println("analytic estimation " + E)

  }
}
