object Chernoff extends App{

  import scala.util.Random._
  import scala.math._
  import scala.annotation.tailrec

  def prob(p: Double, delta: Double, n: Int, r: Int): Double = {

    val mu: Double = p * n

    @tailrec
    def prob_sum(run: Int, sum: Int = 0): Int = {
      if (run == 0) {
        sum
      }
      else {
        val v = List.fill(n)(math.floorMod(nextInt(), 6)).map(x => if(x==0) 1 else 0)
        val P: Int = if ((1-delta)*mu >= v.sum) 1 else 0
        prob_sum(run - 1, sum + P)
      }
    }

    val prob = prob_sum(r).toDouble / r
    prob
  }

  val n: Int = 600
  val delta: Double = 1.0/5
  val p: Double = 1.0/6
  val prob: Double = prob(p, delta, n, 1000)
  val rs: Double = exp(-delta*delta*p*n.toDouble/2.0)

  println("Probability: " + prob)           // Probability: 0.015
  println("exp(-delta*delta*mu/2) = " + rs) // exp(-delta*delta*mu/2) = 0.13533528323661262

  if (rs >= prob) {
    println("Inequality is true")
  }
  else {
    println("Inequality not true")
  }

}
