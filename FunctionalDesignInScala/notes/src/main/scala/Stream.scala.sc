def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo+"")
  if(lo >= hi) Stream.Empty
  else Stream.cons(lo, streamRange(lo+1, hi))
}

streamRange(1, 10).take(3).toList

// difference between different definition (each one has a print to show when the definition is evaluated)
def expr = {
  val x = { print("x"); 1}
  lazy val y = { print("y"); 2}
  def z = { print("z"); 3}

  z+y+x+z+y+x
}

expr  // xzyz res1: Int = 12

// sqrt with streams

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

// 2 - Adding the "determination criteria" here....
def isGoodEnough(guess: Double, x: Double): Boolean = {
  math.abs((guess * guess - x) / x) < 0.001
}

// 1) sqrtStream(160).take(20).toList

// 2)
sqrtStream(16).filter(isGoodEnough(_, 16)).take(10).toList


def from(n: Int): Stream[Int] = n #:: from(n+1)
// map faster than filter cause don't produce not necessary compute
from(1).map(_ * 2).take(10).toList // take then element and then multiply by x (2)
from(1).filter(_ % 2 == 0).take(10).toList // produce all the element and take all the element that respect the filter
