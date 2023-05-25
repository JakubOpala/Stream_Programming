object Fibonacci {

  import scala.annotation.tailrec

  // Function returns n-th Fibonacci number
  def tailFibonacci(n: BigInt): BigInt = {

    if (n < 1) throw new NumberFormatException()

    @tailrec
    def go(index: BigInt, n1: BigInt, n2:BigInt): BigInt = {
      if (index <= 1) n1
      else go(index - 1, n2, n1 + n2)
    }

    go(n, 0, 1)
  }

  def tryCatch(f: Int => BigInt, n: Int): BigInt = {
    try {
      return f(n)
    } catch {
      case e: NumberFormatException => {
        println("Number cant be less than 1")
        -1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt

    //println(n + " Fibonacci number is: " + tryCatch(tailFibonacci, n))
    print(n + " Fibonacci number is: " + tailFibonacci(n))

  }

}
