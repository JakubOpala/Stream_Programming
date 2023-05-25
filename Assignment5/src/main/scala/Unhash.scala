object Unhash {
  import scala.io.Source
  import scala.util.hashing.MurmurHash3

  def tryCatch(subsequences: Array[String]): String = {
    try {
      subsequences.find(MurmurHash3.stringHash(_) == 320915200).get
    } catch {
      case e: NoSuchElementException => "no string found"
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {

      val file = args(0)
      val fileSource = Source.fromFile(file)
      val words = fileSource.mkString //.toLowerCase().split("\\W+")
      val subsequences = words.sliding(24).toArray


      fileSource.close
      // Some(Curiouser and curiouser!)
      println("Sentence with hash = 320915200: " + tryCatch(subsequences))
    }
    else println("No file")
  }

}
