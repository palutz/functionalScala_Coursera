def msort(xs: List[Int]) : List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fstL, sndL) = xs splitAt n
    merge(msort(fstL), msort(sndL))
  }
}

// and this is the merge
def merge(xs: List[Int], ys: List[Int]): List[Int] = {
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1 ) =>
          if(x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
  }
}

// val ml = List[Int](-10, 2, 6, 31, 21, 100, 10)
val ml0 = List(-10, 2, 6, 31, 21, 100, 10)
val ml1 = List[Int](-10, 2, 6, 31, 21, 100, 10)
val ml =  2 :: 6 :: 31 :: 21 :: 100 :: -10 :: Nil

val res = msort(ml)