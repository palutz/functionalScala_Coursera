package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  def matchBracket(character : Char) : Int = character match {
    case ')' => -1
    case '(' => 1
    case _   => 0
  }
  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    chars.foldLeft(0)((acc, curr) => {
      if (acc <0) -1
      else matchBracket(curr) + acc
    }) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, total: Int, min: Int): (Int, Int) = {
      val newTotal = matchBracket(chars(idx)) + total

      if (idx+1 < until)
        traverse(idx+1,until,newTotal, math.min(newTotal, min))
      else
        (newTotal, math.min(newTotal, min))
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until-from > threshold) {
        val middle = from + (until - from) / 2
        val (a, b) = parallel(reduce(from, middle), reduce(middle, until))
        (a._1 + b._1, math.min(a._2, a._1 + b._2))
      }
      else
        traverse(from, until,0,0)
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
