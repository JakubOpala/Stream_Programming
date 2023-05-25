import scala.language.postfixOps

object Misra {

  import scala.io._
  import scala.math._

  def misra(S: Array[String], k: Int): Map[String, Int] = {//Array[String] = {

    var counts = Map.empty[String, Int]

    S.foreach(x => {
      if (counts.contains(x)) {
        counts = counts.updated(x, counts(x) + 1)
      } else if (counts.size < k - 1) {
       counts = counts.updated(x, 1)
      }
      else {
        counts = counts.mapValues(x => x-1).toMap
      }

      counts = counts.filterNot {case (_, count) => count <= 0}.map { case (k, v) => k -> v }

    })

    counts // .mapValues(_.toInt).toMap
  }

  def main(args: Array[String]): Unit = {

    val file1 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/canterbury/alice29.txt" //args(0)
    //val file2 = "C:/Users/opala/IdeaProjects/Assignment9/canterbury-corpus/large/bible.txt" //args(1)
    val fileSource1 = Source.fromFile(file1)
    val words1 = fileSource1.mkString.toLowerCase().split("\\W+").filter(_.nonEmpty)


    val k = 21
    val misra_freqs = misra(words1, k)

    var frequencies: Map[String, Int] = Map.empty[String, Int]
    var lower_frequencies: Map[String, Int] = Map.empty[String, Int]
    val threshold = words1.length/k

    val col_width: Array[Int] = Array("Word".length, "Frequency".length, "Lower frequency bound".length, "Misra-Gries frequency".length, "Misra-Gries frequency < threshold".length, "Theoretical estimate".length)

    var word_count = 0

    misra_freqs.keys.foreach(word => {
      col_width(0) = max(col_width(0), word.length)

      word_count = words1.count(_ == word)
      col_width(1) = max(col_width(1), word_count.toString.length)
      frequencies = frequencies.updated(word, word_count)

      lower_frequencies = lower_frequencies.updated(word, word_count - threshold)
      col_width(2) = max(col_width(2), (word_count - threshold).toString.length)

      col_width(3) = max(col_width(3), misra_freqs.get(word).getOrElse(0).toString.length)
      word
      //println(word + "   |   " + frequency.toString + "   |   " + low_frequency + "   |   " + freqs.get(word))
    })

    val indent = 3
    println("Threshold = " + threshold)
    println("-------" + "-" * (col_width.sum + 6 * indent))

    var space1 = " " * (col_width(0) - "Word".length + indent)
    var space2 = " " * (col_width(1) - "Frequency".length + indent)
    var space3 = " " * (col_width(2) - "Lower frequency bound".length + indent)
    var space4 = " " * (col_width(3) - "Misra-Gries frequency".length + indent)
    var space5 = " " * indent
    var space6 = " " * indent

    println("|" + space1 + "Word|" + space2 + "Frequency|" + space3 + "Lower frequency bound|" + space4 + "Misra-Gries frequency|" + space5 + "Misra-Gries frequency > threshold|" + space6 + "Theoretical estimate|")
    println("-------" + "-" * (col_width.sum + 6 * indent))

    var i = 0

    //val misra_sorted = misra_freqs.keys.zip(frequencies).toMap.toSeq.sortBy(- _._2).toMap
    val frequencies_sorted = frequencies.toSeq.sortBy(- _._2).map(_._1)


    frequencies_sorted.foreach(word => {
      space1 = " " * (col_width(0) - word.length + indent)
      space2 = " " * (col_width(1) - frequencies(word).toString.length + indent)
      space3 = " " * (col_width(2) - lower_frequencies(word).toString.length + indent)
      space4 = " " * (col_width(3) - misra_freqs.get(word).getOrElse(-1).toString.length + indent)
      space5 = " " * (col_width(4) - (misra_freqs.get(word).getOrElse(-1) < threshold).toString.length + indent)
      space6 = " " * (col_width(5) - (misra_freqs.get(word).getOrElse(-1) < frequencies(word) && frequencies(word) - threshold < misra_freqs.get(word).getOrElse(-1)).toString.length + indent)
      println("|" + space1 + word + "|" + space2 + frequencies(word) + "|" + space3 + lower_frequencies(word) + "|" + space4 + misra_freqs.get(word).getOrElse(-1).toString + "|" + space5 + (misra_freqs.get(word).getOrElse(-1) > threshold).toString + "|" + space6 + (misra_freqs.get(word).getOrElse(-1) >= (frequencies(word) - threshold) && misra_freqs.get(word).getOrElse(-1) <= threshold).toString + "|")
      println("-------" + "-" * (col_width.sum+ 6 * indent))
      i += 1
    })

    //println("-------" + "-" * (col_width(0) + col_width(1) + col_width(2) + col_width(3)))

    fileSource1.close


  }

}


/* Results

--------------------------------------------------------------------------------------------------------------------------------------------
|          Word|   Frequency|   Lower frequency bound|   Misra-Gries frequency|   Misra-Gries frequency > threshold|   Theoretical estimate|
--------------------------------------------------------------------------------------------------------------------------------------------
|           the|        1642|                     341|                     384|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|           and|         872|                    -429|                       6|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|            of|         513|                    -788|                       4|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|            in|         369|                    -932|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|           her|         247|                   -1054|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|           all|         182|                   -1119|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|          with|         180|                   -1121|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|         would|          83|                   -1218|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|         their|          52|                   -1249|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|           end|          18|                   -1283|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|          life|          12|                   -1289|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|         child|          11|                   -1290|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|           own|          10|                   -1291|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|        simple|           5|                   -1296|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|          days|           4|                   -1297|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|        summer|           2|                   -1299|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|      pleasure|           2|                   -1299|                       1|                                false|                   true|
--------------------------------------------------------------------------------------------------------------------------------------------
|         happy|           1|                   -1300|                       1|                                false|                  true|
--------------------------------------------------------------------------------------------------------------------------------------------
|          joys|           1|                   -1300|                       1|                                false|                  true|
--------------------------------------------------------------------------------------------------------------------------------------------
|   remembering|           1|                   -1300|                       1|                                false|                  true|
--------------------------------------------------------------------------------------------------------------------------------------------

 */