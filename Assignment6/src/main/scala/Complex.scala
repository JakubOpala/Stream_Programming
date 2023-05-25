import scala.math.sqrt

class Complex (a: Double, b:Double) {

  import scala.math._
  import scala.language.implicitConversions

  //implicit def doubletoComplex(x: Double) = new Complex(x)

  override def toString: String = s"$a + $b i"

  val real: Double = a
  val imaginary: Double = b

  def this(a: Double) = this(a, 0)

  def add(that: Complex): Complex =
    new Complex(that.real + real, that.imaginary + imaginary)

  def abs(): Double = {
    sqrt( real * real + imaginary * imaginary )
  }

  def conj(): Complex = {
    new Complex(real, -imaginary)
  }

  def +(that: Complex): Complex =
    new Complex(that.real + real,  imaginary + that.imaginary)

  def +(n: Double): Complex =
    new Complex(n + real, imaginary)

  def -(that: Complex): Complex =
    new Complex(real - that.real, imaginary - that.imaginary)

  def -(n: Double): Complex =
    new Complex(real - n, imaginary)

  def * (that: Complex): Complex =
    new Complex(that.real * real - that.imaginary * imaginary, that.real * imaginary + real * that.imaginary)

  def * (n: Double): Complex =
    new Complex(n * real, n * imaginary)

  def / (that: Complex): Complex =
    new Complex((that.conj() * this).real / (this.abs()*this.abs()), (that.conj() * this).imaginary / (this.abs()*this.abs()))

  def / (n: Double): Complex =
    new Complex(this.real/n, this.imaginary/n)

}

object Complex {
  
  def apply(a: Int, b: Int = 0)= new Complex(a, b)

  def conj(x: Complex): Complex = x.conj()

  def abs(x: Complex): Double = x.abs()

}

