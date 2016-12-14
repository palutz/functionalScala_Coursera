import scala.collection.parallel._

def sum(xs: Array[Int]) : Int = {
  xs.par.foldLeft(0)(_ + _)
}

sum(Array(1,2,3,4,5,6))

