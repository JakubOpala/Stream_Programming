object Sum {
  def main(args: Array[String]): Unit = {
    def amicableSum(m: Int): Int = {
      def amicable(x: Int): Boolean = {
        val y = divisorsSum(x)
        x != y && x == divisorsSum(y)
      }

      def divisorsSum(x: Int): Int = {
        (1 to x / 2).filter(x % _ == 0).sum
      }
      val startTime = System.nanoTime()
      val s =(1 until m).filter(amicable).sum
      val endTime = System.nanoTime()
      val Time = (endTime - startTime) / 1000000.0
      println(s"Time for m= $m: $Time ms")
      return s
    }
    // We can see that relation between time of calculation and m parameter is quadratic t~n^2
    val m: Int = args(0).toInt
    println("Sum of amicable numbers under " + m + ": " + amicableSum(m))
    println("Sum of amicable numbers under " + 10*m + ": " + amicableSum(10*m))
    println("Sum of amicable numbers under " + 100*m + ": " + amicableSum(100*m))
    println("Sum of amicable numbers under " + 1000*m + ": " + amicableSum(1000*m))
  }
}
