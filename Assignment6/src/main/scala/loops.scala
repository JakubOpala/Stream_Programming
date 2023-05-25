object loops {


  def main(args: Array[String]): Unit = {
    val range = 2 to 100
    //for (i <- range if i % 2 != 0; if i % 3 != 0; if i % 5 != 0) yield (i * i)
    val res = range.withFilter(x => x % 2 != 0 && x % 3 != 0 && x % 5 != 0).map(x => x * x)


    //for (i <- range if i % 2 != 0; j <- range if j % 2 == 0) yield (i, j)
    val res2 = range.withFilter(_ % 2 != 0).flatMap(x => range.withFilter(_ % 2 == 0).map(y => (x, y)))

    print(res)
    print(res2)

  }
}
