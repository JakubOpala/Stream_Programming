object Task3 extends App{

  import scala.util.Random._
  import scala.math._
  import scala.annotation.tailrec

  def median(S: List[Double]): Double = {
    if (S.length % 2 == 0) (S(S.length / 2 + 1) + S(S.length / 2 - 1)) / 2
    else S((S.length - 1) / 2)
  }

  def prob1(s: Int, r: Int): Double = {

    //val mu: Double = 1/2

    @tailrec
    def prob_sum1(r: Int, sum: Double = 0.0): Double = {
      if (r == 0) {
        sum
      }
      else {
        val v = List.fill(s)(nextDouble()).sorted
        val mu: Double = v.sum / s
        val Y: Double = median(v)
        val P: Double = if (abs(Y - mu) > 1.0 / 6) 1 else 0
        println(Y + "    " + abs(Y - mu) + "    " + P)
        prob_sum1(r - 1, sum + P)
      }
    }

    val prob = prob_sum1(r) / r
    prob
  }

  def prob2(s: Int, r: Int): Double = {

    //val mu: Double = 1 / 2

    @tailrec
    def prob_sum2(r: Int, sum: Double = 0.0): Double = {
      if (r == 0) {
        sum
      }
      else {
        val v = List.fill(s)(nextDouble())
        val mu = 1.0 / 2 //v.sum / s
        val Z = v.count(x => 1.0/6 >= abs(x-mu)).toDouble
        val P: Double = if (Z < s.toDouble/2) 1 else 0
        //println(Z)
        prob_sum2(r - 1, sum + P)
      }
    }

    val prob = prob_sum2(r).toDouble / r
    prob
  }



  val s: Int = 600
  val r: Int = 1000
  val prob1: Double = prob1(s, r)
  val prob2: Double = prob2(s, r)

  println("P(|Y-u|>1/6) = " + prob1) // P(|Y-u|>1/6) = 0.0
  println("P(Z < s/2) = " + prob2) // P(Z < s/2) = 1.0

  if (prob2 >= prob1) {
    println("Inequality is true") // Inequality is true
  }
  else {
    println("Inequality not true")
  }

}
