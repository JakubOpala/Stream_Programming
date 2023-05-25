import Element.elem

object Rectangle {
  val space = elem(" ")
  val corner = elem("+")

  def rectangle(w: Int, h: Int): Unit = {

    if(w < 2 || h < 2){
      println("Wrong input, width and height must not be less than 2")
    }
    else{
      def verticalBar = elem('|', 1, h - 2)

      def horizontalBar = elem('=', w - 2, 1)

      def interior: Element = elem(' ', w - 2, h - 2)

      if (w > 2 && h > 2) {
        println((corner beside horizontalBar beside corner) above (verticalBar beside interior beside verticalBar) above (corner beside horizontalBar beside corner))
      } else if (w > 2) {
        println((corner beside horizontalBar beside corner) above (corner beside horizontalBar beside corner))
      } else if (h > 2) {
        println((corner beside corner) above (verticalBar beside verticalBar) above (corner beside corner))
      } else {
        println((corner beside corner) above (corner beside corner))
      }
    }
  }

  def main(args: Array[String]) = {
    val width = args(0).toInt
    val height = args(1).toInt
    rectangle(width, height)
  }
}