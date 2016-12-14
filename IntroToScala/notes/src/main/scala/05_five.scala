/**
  * Created by stefano.paluello on 28/06/2016.
  */
object five {
  // my merge sort...
  def msort(xs: List[Int]) : List[Int] = {
    if (xs.length == 0) xs
    else {
      val n = xs.length / 2
      val (firstL, secondL) = xs splitAt n
      merge(msort(firstL), msort(secondL))
    }
  }

  // and this is the merge
  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    xs match {
      case Nil => ys
      case x1 :: xs1 =>
        ys match {
          case Nil => xs
          case y1 :: ys1 =>
            if(x1 < y1) x1 :: merge(xs1, ys)
            else y1 :: merge(xs, ys1)
        }
    }
  }

  // val ml = List[Int](-10, 2, 6, 31, 21, 100, 10)
  val ml0 = List(-10, 2, 6, 31, 21, 100, 10)
  val ml1 = List[Int](-10, 2, 6, 31, 21, 100, 10)
  val ml =  2 :: 6 :: 31 :: 21 :: 100 :: -10 :: Nil

  val res = msort(ml)
}
