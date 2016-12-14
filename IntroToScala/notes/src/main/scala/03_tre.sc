/*
trait MySet[T] {
  def incl(x: T) : MySet[T]
  def contains(x: T) : Boolean
  def union(other: MySet[T]) : MySet[T]
}
*/

trait IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet) : IntSet
}

// or I can use the abstract class...
/*
abstract class IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet) : IntSet
}
*/

class Empty extends IntSet {
  def contains(x: Int) : Boolean = false // not contain any value
  def incl(x: Int) : IntSet = new NonEmpty(x, new Empty, new Empty) // returns a new set with the new value
  def union(other: IntSet) : IntSet = other
  override def toString = "_"
}

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }
  def contains(x: Int): Boolean = {
    if(x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def union(other: IntSet) : IntSet = {
    ((left union right) union other) incl elem
  }
  override def toString: String = "{ " + left + elem + right + " }"
}


val ee = new Empty
val uu = ee incl 7
val dd = uu incl 3
val tt = dd incl 9
tt incl 11 incl 5


