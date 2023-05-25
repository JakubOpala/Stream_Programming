object DistinctWords {

  import scala.io._

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {
      // We take contents of file as one big string and split it to get array of strings - words
      val file = args(0)
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString.toLowerCase().split("\\W+")
      // We use distinct method to eliminate multiple-times occuring words
      val count = words.distinct.length

      fileSource.close
      println("number of distinct words: " + count)
    }
    else println("no file")
  }


}
