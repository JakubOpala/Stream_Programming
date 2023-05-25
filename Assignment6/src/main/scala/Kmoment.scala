object Kmoment {

  import scala.io._

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
      val mom3: BigInt = wordCounts.mapValues(x => x * x * x).values.sum

      fileSource.close
      // Oth moment is a number of distinct words in a text
      println("0th moment: " + mom0)
      // 1st moment is number of all words in a text
      println("1st moment: " + mom1)
      println("2nd moment: " + mom2)
      println("3rd moment: " + mom3)
    }
    else println("no file")
  }
}
