import common._

class week2 {
}

object week2 {

  val threshold = 6

  object reduce {
    // parallel implementation of reduce
    // (not completely functional)
    def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
      if (right - left < threshold) {
        var res = inp(left);
        var i = left + 1
        while (i < right) {
          res = f(res, inp(i))
          i = i + 1
        }
        res
      } else {
        val mid = left + (right - left) / 2
        val (a1, a2) = parallel(
          reduceSeg(inp, left, mid, f),
          reduceSeg(inp, mid, right, f))
        f(a1, a2)
      }
    }

    // Parallel   Reduce on array
    def reduce[A](inp: Array[A], f: (A, A) => A): A =
    reduceSeg(inp, 0, inp.length, f)
  }

  object mapping {

    def mapASegSeq[A,B](inp: Array[A], f : A => B, left: Int, right: Int, out: Array[B]) = {
      var i= left
      while (i < right) {
        out(i)= f(inp(i))
        i= i+1
      }
    }

    def mapSegPar[A,B](inp:Array[A], left: Int, right: Int, fi: (Int,A) => B, out: Array[B]): Unit = {
      // require f to be pure
      if (right - left < threshold)
        mapASegSeq(inp, fi, left, right, out)
      else {
        val mid = left + (right - left)/2
        val _ = parallel(mapSegPar(inp, left, mid, fi, out),
          mapSegPar(inp, mid, right, fi, out))
      }
    }
  }

}