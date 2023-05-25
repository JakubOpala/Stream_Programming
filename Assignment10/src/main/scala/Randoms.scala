import scala.math._
import scala.util.Random.nextInt

object Randoms extends App{

  def median(S: Array[Int]): Double = {
    if (S.length % 2 == 0) S(S.length/2 + 1) + S(S.length/2 - 1)
    else S((S.length - 1)/2)
  }

  def ar_mean(S: Array[Int]): Double = {
    S.sum / S.length
  }

  def quad_mean(S: Array[Int]): Double = {
    sqrt(S.map(x => x.toDouble * x.toDouble).sum / S.length)
  }

  def cube_mean(S: Array[Int]): Double = {
    cbrt(S.map(x => x.toDouble * x.toDouble * x.toDouble).sum / S.length)
  }

  val m: Int = pow(2,20).toInt + 1
  val randoms = Array.fill(m)(nextInt(m)).sorted
  //val randoms  = Array(2,3,4,5)

  val arithmetic = ar_mean(randoms)
  val quadratic = quad_mean(randoms)
  val cubic = cube_mean(randoms)

  println("Median: " + median(randoms))
  println("Arithmetic mean: " + arithmetic)
  println("Quadratic mean: " + quadratic)
  println("Cubic mean: " + cubic)

  if (arithmetic < quadratic && quadratic < cubic) println("M1 =< M2 =< M3 inequality is fulfilled")
  else println("M1 =< M2 =< M3 inequality is not fulfilled")

  /* For (0,maxInt) interval
  Median: 1.074671557E9
  Arithmetic mean: 539.0
  Quadratic mean: 1.240302793326227E9
  Cubic mean: 1.3532564520897868E9
  M1 =< M2 =< M3 inequality is fulfilled
   */

  /* For (0,m) interval
  Median: 525004.0
  Arithmetic mean: 391.0
  Quadratic mean: 605788.8686320698
  Cubic mean: 660938.333999733
  M1 =< M2 =< M3 inequality is fulfilled
   */

}
