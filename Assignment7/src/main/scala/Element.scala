object Element {

  import scala.math._

  private class ArrayElement(
                              val contents: Array[String]
                            ) extends Element

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override def height = 1
  }

  private class UniformElement(
                                ch: Char,
                                override val width: Int,
                                override val height: Int
                              ) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }


  //Added subtype of Element for representing rational numbers (fraction)
  private class Fraction(num: Int, div: Int = 1) extends Element {
    override def height: Int = 3

    override def width: Int = max(num.toString.length, div.toString.length)
    private val line = "-" * width
    private val space1 = " " * max(0,((width-num.toString.length)/2).toInt)
    private val space2 = " " * max(0,((width-div.toString.length)/2).toInt)
    def contents = Array(space1 + num.toString, line, space2 + div.toString)

  }

  //Added subtype of Element for representing integers
  private class Number(num: Int) extends Element {
    override def height: Int = 1
    override def width: Int = num.toString.length
    def contents = Array(num.toString)
  }

  def elem(contents:  Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element = new LineElement(line)

  //factory object for making new fractions
  def elem(num: Int, div: Int): Element = new Fraction(num, div)

  //factory object for making new integers
  def elem(num: Int): Element = new Number(num)
}

import Element.elem

abstract class Element {
  def contents:  Array[String]
  def width: Int = contents(0).length
  def height: Int = contents.length

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(
      for ((line1, line2) <- this1.contents zip that1.contents)
        yield line1 + line2)
  }

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }

  def +(that: Element): String = (this beside elem("+") beside that).toString

  override def toString = contents mkString "\n"

}