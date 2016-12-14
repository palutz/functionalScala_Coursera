import expression.{Number, Sum}

/**
  * Created by stefano.paluello on 14/06/2016.
  */

object expression {
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  def eval(e: Expr): Int = {
    e match {
      case Number(n) => n
      case Sum(e1, e2) => eval(e1) + eval(e2)
    }
  }
  // receive an expression and return the string representation of the expression
  def show(e: Expr): String = {
    e match {
      case Number(n) => n.toString
      case Sum(e1, e2) => show(e1) + " + " + show(e2)
    }
  }

  show(Sum(Number(1), Number(20)))
}

// 2nd PAttern matching exercise: print the minimum number of parentheses as possible
// HINT - check the precedence of the operator
object pattermatching {
  trait Expr
  case class Number(n: Int) extends Expr
  case class Var(x: String) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = {
    e match {
      case Number(n) => n
      case Sum(e1, e2) => eval(e1) + eval(e2)
      case Prod(e1, e2) => eval(e1) * eval(e2)
    }
  }
  // receive an expression and return the string representation of the expression
  def show(e: Expr): String = {
    e match {
      case Number(n) => n.toString
      case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
      case Prod(e1, e2) => show(e1) + " * " + show(e2)
    }
  }

  show(Sum(Number(1), Number(20)))
}
