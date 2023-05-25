object Majority extends App {
  val S = Array('a', 'a', 'a', 'c', 'c', 'b', 'b', 'c', 'c', 'c', 'b', 'c', 'c')

  var T: Option[Char] = None
  var n = 0
  println("(T, n)")
  println("-------")
  println(T, n)
  S.foreach(x => {
    if (n == 0) {
      T = Some(x)
      n = 1
    } else if (T.get == x) n += 1
    else n -= 1

    println(T.get, n)
  })
}
