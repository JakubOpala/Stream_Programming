object Monad {




  def main(args: Array[String]): Unit = {

    val f = (i: Int) => Option(i * i)
    val g = (i: Int) => Option(i * i * i)
    val m = Some(2)
    //val m = None

    val l1 = (m flatMap f) flatMap g
    val r1 = m flatMap (x => f(x) flatMap g)

    // prints (Some(64),Some(64)) for m = Some(2), and (None, None) for m = None which shows associativity
    println(l1, r1)

    //val l2 = Some(m) flatMap f
    //val r2= f(2)

    //println(l2, r2)


  }
}
