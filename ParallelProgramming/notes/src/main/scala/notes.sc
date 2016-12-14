import scala.annotation.tailrec

/**
  *
  * Calculate p-norm (sum segment): sum from i=s to t-1 of (a(i)^p)
  * the recursive way.. :P
  * @param a - the array with all the elements
  * @param p - is the exponent
  * @param s
  * @param t
  * @return
  */
@tailrec
def sumSegment(a: Array[Int], p: Double, s: Int, t: Int, acc: Int): Int = {
  if ((t - s) > 0) {
    val calc = Math.pow(a(s).toDouble, p).toInt
    sumSegment(a, p, s + 1, t, acc + calc)
  } else
    acc
}

sumSegment(Array(1,2,3), 2.0, 0, 3, 0)

// "sequential version"
def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s
  var sum: Int = 0
  while(i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}
def power(x: Int, p: Double): Int = math.exp(p * math.log((math.abs(x)))).toInt
// pNorm = sumSegment^(1/p)
val pNorm = power(sumSegment(Array(1,2,3), 2.0, 0, 3), 1/2.0)

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val hl = a.length / 2
  val (v1, v2)= common.parallel(sumSegment(a, p, 0, hl), sumSegment(a, p, hl, a.length))

  power(v1 + v2, 1/2.0)
}