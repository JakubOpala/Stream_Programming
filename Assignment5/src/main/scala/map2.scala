object map2 {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      op_a <- a
      op_b <- b
    } yield f(op_a,op_b)

  def main(args: Array[String]): Unit = {

    val a: Option[Int] = Some(2)
    val b: Option[Int] = Some(4)
    val name: Option[String] = Some("thing")
    val surname: Option[String] = Some("in the way")
    val nothn: Option[Int] = None
    val nothn2: Option[Int] = None

    val power: Option[Int] = map2(a,b) { (a,b) => a*b}
    val words: Option[String] = map2(name,surname) { (a,b) => a+" "+b }
    val nothin: Option[Int] = map2(a,nothn) { (a,b) => a*b}
    val double_nothin: Option[Int] = map2(nothn,nothn2) { (a,b) => a*b}

    // Test for 2 some() arguments
    println("2 times 4 is: " + power )
    println("top nirvana song: " + words)
    // Test for 1 some() and 1 None argument
    println("1None: " + nothin)
    // Test for 2 None arguments
    println("2None: " + double_nothin)

  }
}
