import Element.elem
import Rational.frac

object Fractions {

  def main(args: Array[String]): Unit = {

    if (args.length == 2){
      val num = args(0).toInt
      val div = args(1).toInt


      val lhs: Element = elem(frac(num, div)) + elem(frac(2, 5))
      val rhs: Element = elem(frac(num, div)+frac(2, 5))

      println(lhs equals rhs)
    }
    else if (args.length == 1){
      val num = args(0).toInt

      val lhs: Element = elem(frac(num)) + elem(frac(2, 5))
      val rhs: Element = elem(frac(num) + frac(2, 5))

      println(lhs equals rhs)
    }

  }
}
