package recfun

object Main {
  def main(args: Array[String]) {
    /*
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    */
    //val res = balance("(if (zero? x) max (/ 1 x))".toList)
    // val res = balance("())(".toList)
    //val res = balance(":-)".toList)
    //println(s"res=$res")
    //println(countChange(4, List(1,2,3)))
    //println(countChange(4, List(2,3,1)))
    //println(countChange(0, List(1,2,3)))
    //println(countChange(2, List(3,4,5)))
    //println(countChange(20, List(3,4,5)))
  }

  /**
   * Exercise 1 - Pascal Triangle
    *
    * The following pattern of numbers is called Pascal’s triangle.

          1
         1 1
        1 2 1
       1 3 3 1
      1 4 6 4 1
     ...
    The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
    Write a function that computes the elements of Pascal’s triangle by means of a recursive process.

    Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r, counting from 0 and returns the number at that spot in the triangle.
    For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
    *
    *
   */
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case (c ,r) if (c == 0) || (c == r) => 1
      case (c ,r) if (c < 0) || (c > r ) => 0
      case (_, _) => pascal(c-1, r-1) + pascal(c, r-1)
    }
  }
  
  /**
   * Exercise 2
    *
    * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String.

    For example, the function should return true for the following strings:
    (if (zero? x) max (/ 1 x))
    I told him (that it’s not (yet) done). (But he wasn’t listening)
    The function should return false for the following strings:

    :-)
    ())(
    The last example shows that it’s not enough to verify that a string contains the same number of opening and closing parentheses.
   */
  def balance(chars: List[Char]): Boolean = {
    def counterRec(cList: List[Char], counter: Int, counting: Boolean) : Boolean =
      if(counter >= 0) {
        if(cList.isEmpty) (counting && counter == 0)
        else cList.head match {
          case '(' => counterRec(cList.tail, counter + 1, true)
          case ')' => counterRec(cList.tail, counter -1, true)
          case _ => counterRec(cList.tail, counter, true)
        }
      } else
        false
    counterRec(chars, 0, false)
  }
  
  /**
   * Exercise 3
    * Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations.
    *
    * For example, there are 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]) : Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted)
  }

}
