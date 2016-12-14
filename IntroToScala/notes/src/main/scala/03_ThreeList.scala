/**
  * Created by palutz on 05/06/16.
  */
object ThreeList {
  def nth[T](i: Int, xs: MyList[T]): T = {
    if(xs.isEmpty) throw new IndexOutOfBoundsException
    else if(i == 0) xs.head  // for a 0 bsed list. i == 1 for a 1 based/starting list
    else nth(i - 1, xs.tail)
  }


  val ml = new Cons(0, new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, new Nul[Int]()))))))
  val a = nth(3, ml)
  val b = nth(6, ml)  // IndexOutofBound
  val c = nth(1, ml)  // IndexOutofBound
}

trait MyList[T] {
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  def isEmpty: Boolean = false
}

class Nul[T] extends MyList[T] {
  override def isEmpty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.head")
  override def head: Nothing = throw new NoSuchElementException("Nil.tail")
}