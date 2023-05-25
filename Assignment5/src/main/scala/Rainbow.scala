object Rainbow {

  import scala.annotation.tailrec
  import scala.math._
  import scala.util.hashing.MurmurHash3

  val alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  def reduction(n: Int): String = {
    val reduce = new StringBuilder(4)

    @tailrec
    def add_lett(sb: StringBuilder, n: Int, index: Int): String = {
      if (index < 1) {
        sb.toString()
      }
      else {
        if (index == 1){
          sb.append(abs(floorMod(n, 128)).toChar)     //(n % (alphabet.length * index)))
        }
        else{
          sb.append(abs(floorMod((n/pow(128,index-1)).toInt,128)).toChar)
        }
        add_lett(sb, n, index - 1)
      }
    }

    add_lett(reduce, n, 4)
  }

  @tailrec
  def rainbowset(rainbow_table: Array[String], n: Int): Array[String] = {
    if (n < 1) rainbow_table
    else {
      val H = MurmurHash3.stringHash(rainbow_table.last)
      val P1 = reduction(H)
      val rainbow_table_next = rainbow_table :+ P1
      rainbowset(rainbow_table_next, n - 1)
    }
  }


  def main(args: Array[String]): Unit = {

    val n = 100 //args(0).toInt
    val P_0 = "AAAA"
    val rainbow_table: Array[String] = Array(P_0)
    println("Password corresponding to n = " + n + " is: " + rainbowset(rainbow_table, n).last)
    //MurmurHash3.stringHash(P_0)

  }

}