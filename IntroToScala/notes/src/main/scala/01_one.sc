// define  && and || without using the symbols
def and(x:Boolean, y: =>Boolean) = if(x) y else false
def or(x:Boolean,y: =>Boolean) = if(x) true else y
and(false, true)
// def loop: Boolean = loop
// and(false, loop)
// and(true, loop) ... infinite loop
or(false, false)
or(true, false)

// **********************
// Compute sqrt with Newton method

def sqrt(num: Double) : Double = {

  def sqrIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else sqrIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean =
    Math.abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double): Double =
    (guess + x / guess) / 2

  sqrIter(1.0, num)
}

sqrt(4)
sqrt(2)

// **************************
//  GCD - greatest common divisor
def gcd(a: Int, b: Int): Int =
  if(b==0) a else gcd(b, a % b)

gcd(0, 14)
gcd(21, 14)


// factorial
def factorial(n: Int) : Int =
  if(n == 0) 1 else (n * factorial(n-1))

factorial(6)

// tailrec factorial recursive
def factorialtailrec(n:Int): Int = {
  @annotation.tailrec
  def innerLoop(x: Int, acc: Int) : Int =
    if(x == 0) acc
    else innerLoop(x-1, x * acc)
  innerLoop(n, 1)
}
factorialtailrec(6)