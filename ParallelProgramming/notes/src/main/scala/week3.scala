import common._
import scala.collection.parallel._

class week3 {  // Splitters and Combiners

  trait Iterator[A] {
    def next(): A
    def hasNext: Boolean

    // foldLeft implemented on an iterator
    def foldLeft[B](z: B)(f:(B, A) => B): B = {
      var s = z
      while(hasNext) s = f(s, next())
      s
    }
  }
  def iterator: Iterator[A]

  trait Splitter[A] extends Iterator[A] {
    def split: Seq[Splitter[A]]
    def remaining: Int

    // implementing fold on a splitter ...
    def fold(z: A)(f: (A, A) => A): A = {
      val threshold = 6
      if (remaining < threshold) foldLeft(z)(f)
      else {
        val children = for(child <-split) yield task { child.fold(z)(f) }
        children.map(_.join()).foldLeft(z)(f)
      }
    }
  }
  def splitter: Splitter[A]

  trait Builder[A, Repr] {
    def +=(elem: A): Builder[A, Repr]
    def result: Repr
  }
  def newBuilder: Builder[A, Repr]   // Builder[Int, List[Int]]

  trait Traversable[T] {
    def foreach(f: T => Unit): Unit
    def newBuilder: Builder[T, Traversable[T]]

    def filter(p: T => Boolean): Traversable[T] = {
      val b = newBuilder
      for (x <- this) yield(if(p(x)) b += x )
      b.result
    }
  }


  trait Combiner[A, Repr] extends Builder[A, Repr] {
    def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
  }
  def newCombiner: Combiner[T, Repr]

  // parallel filter with splitter and newCombiner
  def pfilter(p: T => Boolean): Traversable[T] = ???
}