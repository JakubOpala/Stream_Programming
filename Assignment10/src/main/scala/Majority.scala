object Majority extends App {
  val S = Array('a', 'a', 'a', 'c', 'c', 'b', 'b', 'c', 'c', 'c', 'b', 'c', 'c', 'd', 'c', 'd', 'c')

  var T: Option[Char] = None
  var n = 0

  println("(T, n)")
  println("-------")
  println(T, n)
  S.foreach(x => {
    if (n == 0) {
      T = Some(x)
      n = 1
    } else if (T.get == x) {
      n += 1
    }
    else {
      n -= 1
    }

    println(T.get, n)
  })

  println("Rerun with check: ")
  val T_S = T.get
  T = None
  n = 0
  var n_p = 0
  var n_p2 = 0


  S.foreach(x => {
    if (n == 0) {
      T = Some(x)
      n = 1
    } else if (T.get == x) {
      n += 1
    }
    else {
      n -= 1
    }

    if (x == T_S) {
      n_p2 += 1
      n_p = n
    }
    else {
      n_p2 -= 1
      n_p = -n
    }

    println(T.get, n, n_p, n_p2)
  })

  // Majority algorithm returns as candidate for majority 'c' with n=3 at the end
  // After rerun with n' it reaches value 1 at the end which is a positive number - it confirms that c is a major element
  // This method of validating the output of majority algorithm works because each occurence of the potential majority element
  // increases n' by one and each occurence of other element decreases it by one. This means that if our element is a majority
  // there will be more increases than decreases - final n' value will be bigger than 0

  /*
  (T, n)
  -------
  (None,0)
  (a,1)
  (a,2)
  (a,3)
  (a,2)
  (a,1)
  (a,0)
  (b,1)
  (b,0)
  (c,1)
  (c,2)
  (c,1)
  (c,2)
  (c,3)
  (c,2)
  (c,3)
  (c,2)
  (c,3)
  Rerun with check:
  (T, n, n', n'')
  (a,1,-1,-1)
  (a,2,-2,-2)
  (a,3,-3,-3)
  (a,2,2,-2)
  (a,1,1,-1)
  (a,0,0,-2)
  (b,1,-1,-3)
  (b,0,0,-2)
  (c,1,1,-1)
  (c,2,2,0)
  (c,1,-1,-1)
  (c,2,2,0)
  (c,3,3,1)
  (c,2,-2,0)
  (c,3,3,1)
  (c,2,-2,0)
  (c,3,3,1)

   */

}
