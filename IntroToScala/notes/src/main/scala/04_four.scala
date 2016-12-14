/**
  * Created by stefano.paluello on 07/06/2016.
  */
object four {
  // natural number only OBJ implementation
  // Peano numbers
  abstract class Nat {
    def isZero : Boolean
    def predecessor : Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat) : Nat
    def - (that: Nat) : Nat
  }

  object Zero extends Nat {
    override def isZero: Boolean = true
    override def predecessor: Nat = throw new Error("0.prtedecessor")
    override def +(that: Nat): Nat = that
    override def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false
    override def predecessor: Nat = n
    override def +(that: Nat): Nat = new Succ(n + that)
    override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
  }
}

/*
object List {
  def insert(x: Int, xs: List[Int]) : List[Int] =
    xs match {
      case List() => List(x)
      case y :: ys => if(x <= y) x::xs else y::insert(x, ys)
    }
}
*/
