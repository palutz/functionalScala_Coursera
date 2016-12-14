/**
  * Get a Stream of prime number using the Sieve of Eratosthenes' algorithm:
  * We start from 2 (the first prime number and eliminate all the multiples of 2
  * Then we move to the next available number, 3, another prime number and we repeat
  * the same action (remove all the multiples). And so on.. (next 5, multiples, then next 7 and so on... )
  */
object primes {

  def from(n: Int): Stream[Int] = n #:: from(n+1)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter(_ % s.head != 0))


  val primes = sieve(from(2))
  /* the same of...
  val from2 = from(2)  // from2: Stream[Int] = Stream(2, ?)
  val primes = sieve(from2)  // primes: Stream[Int] = Stream(2, ?)
  */

  primes.take(100).toList  /* res2: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
                    41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
                    127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
                    211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
                    307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397,
                    401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491,
                    499, 503, 509, 521, 523, 541)   */
}

object Sqrt {
  // 1 - doing like this we got a perfectly good stream of approximation to the result
  // decoupling, actually the idea of converging sequence from the "determination criteria" (that we can add after)
  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }
  // 2 - Adding the "determination criteria" here....
  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001

  // 1) sqrtStream(160).take(20).toList

  // 2)
  sqrtStream(16) filter (isGoodEnough(_, 4))
}
