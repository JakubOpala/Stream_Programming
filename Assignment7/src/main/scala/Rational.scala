import Element.elem

object Rational {

  def main(args: Array[String]): Unit = {

    if (args.length == 2){
      val num = args(0).toInt
      val div = args(1).toInt

      println(elem(num, div))
    }
    else if (args.length == 1){
      val num = args(0).toInt
      println(elem(num))
    }

  }
}
