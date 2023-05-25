import scala.io.Source

object MinMaxMoment {
  def minMaxMoment(n0: Int, n1: Int):Array[Int] = {

    // We obtain maximum 2nd momentum by assuming that every word in stream but one occurs only once - maximize sum of squares
    val maxint: Int = n1 - n0 + 1
    val max2moment: Int = maxint*maxint + n0 - 1

    // We obtain minimum 2nd momentum by assuming that occurrence of words is as equal as possible
    val min_el: Int = (n1 / n0).toInt
    val num_max_el: Int = n1 % n0
    val min2moment: Int = min_el * min_el * (n0 - num_max_el) + (min_el + 1) * (min_el + 1) * num_max_el

    Array(min2moment, max2moment)
  }

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {
      val file = args(0)
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString.toLowerCase().split("\\W+")
      // We map our array of words into map consisting of distinct words and number of their occurences
      val wordCounts = words.groupBy(identity).view.mapValues(_.length)
      // We sort our map by number of occurences of word from the most-frequent word and we take top positions
      val mom0 = wordCounts.mapValues(x => 1).values.sum
      val mom1 = wordCounts.values.sum
      val mom2 = wordCounts.mapValues(x => x * x).values.sum
      val minmax: Array[Int] = minMaxMoment(mom0, mom1)

      fileSource.close
      println("Minimal 2nd moment: " + minmax(0))
      println("Maximal 2nd moment: " + minmax(1))
      println("Real 2nd moment: " + mom2)
    }
    else println("no file")
  }

}
