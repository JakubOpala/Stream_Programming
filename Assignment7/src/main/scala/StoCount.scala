import scala.annotation.tailrec

object StoCount {

  import scala.util.Random._
  import scala.math._



  def counting(n: Int):Int = {
    val m: Int = 256
    @tailrec
    def go(M: Array[Int], index: Int):Int = {
      if (index <= 1) {
        println(M.mkString(","))
        M.map(math.pow(2, _).toInt).sum
      }
      // if (index <= 1) M.map(1.0 / _).sum
      else {
        val rho = nextInt()
        val num = ((rho % m) + m) % m
        val rho_zeros = Integer.numberOfLeadingZeros(rho) + 1
        if(rho_zeros > M(num)) M(num) = rho_zeros
        go(M, index - 1)
      }
    }

    val M = Array.fill(256)(0)
    go(M, n)
  }

  def main(args: Array[String]):Unit = {


    println("n = 2^28, 2^M = " + counting(pow(2,28).toInt))

  }
}
