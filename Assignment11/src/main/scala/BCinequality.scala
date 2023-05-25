object BCinequality extends App{

  import scala.util.Random._
  import scala.math._
  import scala.annotation.tailrec

  def prob(k: Double, n:Int, r: Int): Double = {

    @tailrec
    def prob_sum(run: Int, sum: Double = 0): Double = {
      if (run == 0){
        sum
      }
      else {
        val v = List.fill(n)(nextGaussian())
        val mean = v.sum / n
        val variance = v.map(x => pow(x - mean, 2)).sum / n
        val sigma = 1.0 // sqrt(variance)
        val P = v.count(abs(_) >= k * sigma).toDouble / n
        println(variance + " " + " " + sigma + " " + P)
        prob_sum(run - 1, sum + P)
      }
    }

    val prob = prob_sum(r) / r
    prob
  }

  val r: Int = 1000
  val k: Double = 3.0
  val n: Int = 1000000

  val prob: Double = prob(k, n, r)
  val rs = 1/(k*k)
  println("Probability = " + prob) // Probability = 0.002701263000000001
  println("1/k^2 = " + rs)               // 1/k^2 = 0.1111111111111111
                                         //Inequality is true

  if (rs >= prob) {
    println("Inequality is true")
  }
  else {
    println("Inequality not true")
  }





}
