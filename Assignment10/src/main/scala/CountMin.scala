import scala.util.hashing.MurmurHash3
import scala.math._

object CountMin {

  def familyHash(s: String, k: Int): Array[Int] = {
    val hs: Int = MurmurHash3.stringHash(s)
    val (high, low) = (hs >>> 16, hs & 0xFFFF)
    Array.tabulate(k)(j => (high + j * low) & 0xFFFF)
  }

  def counts(words: Array[String], k: Int, familyhash:(String, Int) => Array[Int]): Array[Array[Int]] = {

    var i = 0

    val filter = words.foldLeft(Array.ofDim[Int](k, pow(2,16).toInt).map(row => row.map(_ => 0))) {
      (counts, wi) =>
        //val hashes = familyHash(wi, k)
        //(bits zip hashes).map{case (x, y) => x.max(y)}
        familyHash(wi, k).foldLeft(counts) {
          (counts,hash) =>
            counts(i)(hash) = counts(i)(hash) + 1
            i = i + 1
            counts
        }
        i = 0
        counts
    }
    filter
  }

  def check_word(word: String, filter: Array[Array[Int]], familyhash:(String, Int) => Array[Int]): Int = {
    val hashes = familyhash(word, filter.length)
    //val check = (filter zip hashes).count{ case (x, y) => x != y }

    var i = -1

    val check = hashes.foldLeft(Array.empty[Int]) {
      (acc, hash) =>
        i = i + 1
        acc :+ filter(i)(hash)
    }

    check.min

  }

  def frquencies(A: Array[String]): Map[String, Int] = {

    var i = 0
    val filter = counts(A, 4, familyHash)
    val words_set = A.toSet

    /*
    val freqs = words_set.foldLeft(Array.empty[Int]) {
      (counts, word) =>
        counts :+ check_word(word, filter, familyHash)
    }
    */

    words_set.zip(words_set.toArray.map(check_word(_, filter, familyHash))).toMap
    //words_set.toArray.map(word => check_word(word, filter, familyHash))
  }

  import scala.io._

  def main(args: Array[String]): Unit = {

    val file1 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/canterbury/alice29.txt" //args(0)
    //val file1 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/large/bible.txt" //args(1)

    val fileSource1 = Source.fromFile(file1)

    val words1 = fileSource1.mkString.toLowerCase().split("\\W+").filter(_.nonEmpty)

    val freqs = words1.groupBy(identity).view.mapValues(_.size).toMap // exact frequencies
    val countmin = frquencies(words1) // countmin estimations of frequencies
    //val freqs = words1.zip(words1.groupBy(identity).mapValues(_.size)).toMap

    //println("Words in file" + words1.toSet.take(100))
    println("Precise frequencies of words in text: " + freqs.take(5))
    println("Countmin estimation: " + countmin.take(5))

    var wrong_est = 0

    freqs.keys.foreach(word => {
      if (freqs(word) != countmin(word)) wrong_est += 1
    })

    println("Number of incorrectly estimated frequencies: " + wrong_est)

    /*
    For the alice in wonderland:
    Precise frequencies of words in text: HashMap(est -> 1, sorrows -> 1, beautiful -> 13, furrows -> 1, pulling -> 1)
    Countmin estimation: HashMap(est -> 1, sorrows -> 1, beautiful -> 13, furrows -> 1, pulling -> 1)
    Number of incorrectly estimated frequencies: 0

    For the bible:
    Precise frequencies of words in text: HashMap(confesseth -> 3, brink -> 6, youthful -> 1, healings -> 1, forgotten -> 46)
    Countmin estimation: HashMap(confesseth -> 3, brink -> 6, youthful -> 1, healings -> 1, forgotten -> 46)
    Number of incorrectly estimated frequencies: 11
    */



    // For  Alice in wonderland: Number of incorrectly estimated frequencies: 0
    fileSource1.close

  }
}

