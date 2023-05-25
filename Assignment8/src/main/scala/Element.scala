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
  private class Fraction(n: Int, d: Int = 1) extends Element {
    val num = n / gcd(n, d)
    val div = d / gcd(n, d)

    override def height: Int = 3
    override def width: Int = max(num.toString.length, div.toString.length)

    private val line = "-" * width
    private val space1l = " " * max(0,((width-num.toString.length)/2))
    private val space1r = " " * max(0,width-space1l.length-num.toString.length)
    private val space2l = " " * max(0,((width-div.toString.length)/2))
    private val space2r = " " * max(0,width-space2l.length-div.toString.length)
    def contents = {
      if(div == 1) {
        Array(num.toString)
      }
      else {
        Array(space1l + num.toString + space1r , line, space2l + div.toString + space2r)
      }
    }

    private def +(that: Fraction): ArrayElement = {
      if (this.div != 1 && that.div != 1){
        new ArrayElement(Array(this.contents(0) + "   " + that.contents(0), this.line + " + " + that.line, this.contents(2) + "   " + that.contents(2)))
      }
      else if(this.div == 1){
        val blank1 = new String(List.fill(this.num.toString.length)(" ").mkString)
        new ArrayElement(Array(blank1 + "   " + that.contents(0), this.num + " + " + that.line, blank1 + "   " + that.contents(2)))
      }
      else if (that.div == 1) {
        val blank2 = new String(List.fill(that.num.toString.length)(" ").mkString)
        new ArrayElement(Array(this.contents(0) + "   " + blank2, this.line + " + " + that.num, this.contents(2) + "   " + blank2))
      }
      else{
        new ArrayElement(Array(this.num + " + " + that.num))
      }
    }

  }


  def elem(contents:  Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element = new LineElement(line)

  //factory object for making new fractions
  def elem(num: Int, div: Int): Element = new Fraction(num, div)

  def elem(num: Int): Element = new Fraction(num)

  def elem(q: Rational): Element = new Fraction(q.numer, q.denom)

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

  def equals(that: Element): Element = this beside elem(" = ") beside that

  def +(that: Element): Element = {
    if(this.contents.length == that.contents.length) {
      this beside elem(" + ") beside that
    }
    else if(this.contents.length == 1){
      (elem(" ") above this above elem(" ")) beside elem(" + ") beside that
    }
    else if (that.contents.length == 1){
       this beside elem(" + ") beside (elem(" ") above that above elem(" "))
    }
    else{
      this beside elem(" + ") beside that
    }
  }

  override def toString = contents mkString "\n"

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}