object Main extends App {

  import math._


  // We define lazy list that implements Fibonnacci sequence that stores only last 2 values of it
  val list1: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: list1.zip(list1.tail).map{tab => tab._1 + tab._2}

  // 1.Method that returns index of the smallest Fibonacci number (n) such that its number of digits is
  // greater or equal to the given integer input (d)
  def indexFib(d: Int): Int = {

      //def fib(a: Int, b: Int): LazyList[Int] = a #:: fib(b, a + b)
      //fib(0, 1)

    // We fill LazyList until we reach element, in Fibonacci sequence, that consists of more than d digits
    list1.takeWhile(_.toString.length < d).length
  }

  println("Output for d = 2: " + indexFib(2))
  println("Output for d = 3: " + indexFib(3))
  println("Output for d = 4: " + indexFib(4))
  println("Output for d = 1000: " + indexFib(1000))

}

  // 2. Evaluating the sum of amicable numbers under given m

  /*val initialState = (a, 1) // initialize with a tuple
  val fibs: List[Int] = List.unfold(initialState) {
    case (x, y) if x.length < initialState._1 => { //match the current state
      val result = x // compute the result
      val newState = (y, y + x) // compute the new state
      Some((result, newState))
    }
    case _ => None
  }


  def divisors(a: Int): List[Int] = {
    def divisor(a: Int, n: Int): List[Int] = {
      if (a % n == 0) n :: divisor(a, n+1)
      else divisor(a, n+1)
    }
    for(n <- 1 to a/2)


    )
  }

  val a = List.range(1, 10)
  println("Length of a list:" + lengthRight(a))
}
*/