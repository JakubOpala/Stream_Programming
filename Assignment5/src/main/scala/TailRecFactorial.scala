object TailRecFactorial {

  import scala.annotation.tailrec
  import scala.math._

  def tailFactorial(n: Int): BigInt = {
    if (n<0) throw new NumberFormatException()
    @tailrec
    def go(acc: BigInt, n: Int): BigInt = {
      if (n <= 1) acc
      else go(n * acc, n - 1)
    }
    go(1, n)
  }

  //@tailrec
  def factorial(n: Int): BigInt = {
    if (n<0) throw new NumberFormatException()
    if (n <= 1) 1
    else n * factorial(n-1)
  }

  def tryCatch(f:Int => BigInt,n: Int): BigInt = {
    try {
      return f(n)
    } catch {
      case e: NumberFormatException => -1
      case e: StackOverflowError => -2
    }
  }

  def overflow(f:Int => BigInt, n:Int): Int = {
    if (tryCatch(f,n) == -2) {
      n
    }
    else overflow(f, n+1)
  }

  def main(args:Array[String]):Unit = {
    val n = args(0).toInt

    println("Method 1 factorial: " + tryCatch(factorial, n))
    println("Method 2 factorial: " + tryCatch(tailFactorial, n))
    // Calculated minimal number leading to Stack Overflow: 15580
    //println("Minimal number leading to Stack Overflow: " +  overflow(factorial, 1))

  }
  
}
