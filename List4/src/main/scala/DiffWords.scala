object DiffWords {

  import scala.io._

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {
      // We take all words from both files and we filter set of words from file A and delete every word that is
      // in file B. Then we measure length of the remaining set
      val file1 = args(0)
      val file2 = args(1)
      val fileSource1 = Source.fromFile(file1)
      val fileSource2 = Source.fromFile(file2)
      val words1 = fileSource1.mkString.toLowerCase().split("\\W+")
      val words2 = fileSource2.mkString.toLowerCase().split("\\W+")
      val diff = words1.filterNot(words2.toSet).length


      fileSource1.close
      fileSource2.close
      println("Number of words from A not occuring in B: " + diff)
    }
    else println("no file")
  }


}
