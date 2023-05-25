object MostCommonWords {

  import scala.io._

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {
      val file = args(0)
      val n_words = args(1).toInt
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString.toLowerCase().split("\\W+")
      // We map our array of words into map consisting of distinct words and number of their occurences
      val wordCounts = words.groupBy(identity).view.mapValues(_.length)
      // We sort our map by number of occurences of word from the most-frequent word and we take top positions
      val topNWords = wordCounts.toSeq.sortBy(-_._2).take(n_words)

      fileSource.close
      println(n_words + " most common words: " + topNWords)
    }
    else println("no file")
  }
}
