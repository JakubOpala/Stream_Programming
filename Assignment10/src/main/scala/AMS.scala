import scala.io.Source
import scala.util.Random.nextInt
import scala.math._

object AMS {

  def ams(S: Array[String], r: Int, k: Int): BigInt = {
    val rand_elements = Array.fill(r)(nextInt(S.length))

    val c_arr = rand_elements.map(x => S.drop(x).count(_ == S(x))) //.toLong)
    val mk: BigInt = BigInt(S.length/r) * c_arr.map(x => BigInt(pow(x,k).toInt) - BigInt(pow(x-1,k).toInt)).sum.toInt
    mk
  }

  def main(args: Array[String]): Unit = {
    val file = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/canterbury/alice29.txt" //args(0)
    //val file2 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/large/bible.txt" //args(1)

    val fileSource = Source.fromFile(file)
    val words = fileSource.mkString.toLowerCase().split("\\W+")

    val wordCounts = words.groupBy(identity).view.mapValues(_.length.toLong)

    val mom0 = wordCounts.mapValues(x => 1).values.sum
    val mom1 = wordCounts.values.sum
    val mom2 = wordCounts.mapValues(x => x * x).values.sum
    val mom3: Long = wordCounts.mapValues(x => x * x * x).values.sum

    val r = 60
    val ams_mom2 = ams(words, r, 2)
    val ams_mom3 = ams(words, r, 3)

    fileSource.close

    println("2nd moment: " + mom2)                        // 2nd moment:          7648302
    println("2nd moment with AMS: " + ams_mom2)           // 2nd moment with AMS: 7040670
    println("3rd moment: " + mom3)                        // 3rd moment:          6914019604
    println("3rd moment with AMS: " + ams_mom3.toString)  // 3rd moment with AMS: 5680932985
  }

}
