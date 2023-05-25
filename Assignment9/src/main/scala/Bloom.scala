import scala.util.hashing.MurmurHash3
import scala.math._

object Bloom {

  def familyHash(s: String, k: Int): Array[Int] = {
    val hs: Int = MurmurHash3.stringHash(s)
    val (high, low) = (hs >>> 16, hs & 0xFFFF)
    Array.tabulate(k)(j => (high + j * low) & 0xFFFF)
  }

  def bloom_filter(words: Array[String], k: Int, familyhash:(String, Int) => Array[Int]): Array[Int] = {

    val filter = words.foldLeft(Array.fill(pow(2,16).toInt)(0)) {
      (bits, wi) =>
        //val hashes = familyHash(wi, k)
        //(bits zip hashes).map{case (x, y) => x.max(y)}
        familyHash(wi, k).foldLeft(bits) {
          (bits,hash) =>
            bits.update(hash, 1)
            bits
        }
    }
    filter
  }

  def check_word(word: String, filter: Array[Int], familyhash:(String, Int) => Array[Int]): Int = {
    val hashes = familyhash(word, filter.length)
    //val check = (filter zip hashes).count{ case (x, y) => x != y }

    val check = hashes.foldLeft(0) {
      (acc, hash) => acc + filter(hash) - 1
    }

    if (check < 0){
      1
    }
    else{
      0
    }
  }

  def words_in_A_not_in_B(A: Array[String], B:Array[String]): Int = {
    val filter = bloom_filter(B, 10, familyHash)

    val count = A.foldLeft(0) {
      (acc, word) => acc + check_word(word, filter, familyHash) - 1
    }
    count
  }

  import scala.io._

  def main(args: Array[String]): Unit = {

    val file1 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/canterbury/alice29.txt" //args(0)
    val file2 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/large/bible.txt" //args(1)

    val fileSource1 = Source.fromFile(file1)
    val fileSource2 = Source.fromFile(file2)

    val words1 = fileSource1.mkString.toLowerCase().split("\\W+")
    val words2 = fileSource2.mkString.toLowerCase().split("\\W+")

    val diff = words1.filterNot(words2.toSet).length


    println("Number of words from A not occuring in B: " + diff)
    println("Bloom filter estimation: " + words_in_A_not_in_B(words1, words2))

    fileSource1.close
    fileSource2.close

  }
}
