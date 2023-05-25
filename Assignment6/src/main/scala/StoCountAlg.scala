  import scala.annotation.tailrec

  object StoCountAlg {

    import scala.util.Random._
    import scala.math._


    def counting(n: Int):Int = {
      @tailrec
      def go(M: Int, index: Int):Int = {
        if (index <= 1) M
        else {
          val rho = Integer.numberOfLeadingZeros(nextInt()) + 1
            if(rho > M) go(rho, index - 1)
            else go(M, index - 1)

        }
      }

      go(0, n)
    }

    def main(args: Array[String]):Unit = {

      val M20: Int = counting(pow(2,20).toInt)
      val M24: Int = counting(pow(2,24).toInt)
      val M28: Int = counting(pow(2,28).toInt)

      println("n = 2^20, M = " + M20 + ",  2^M = " + pow(2, M20))
      println("n = 2^24, M = " + M24 + ",  2^M = " + pow(2, M24))
      println("n = 2^28, M = " + M28 + ",  2^M = " + pow(2, M28))

    }
  }
