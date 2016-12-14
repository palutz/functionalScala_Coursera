package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Find the correct minimum") = forAll { (firstElement: A, secondElement: A) =>
    val minimum = if (firstElement < secondElement) firstElement else secondElement
    findMin(insert(secondElement, insert(firstElement, empty))) == minimum
  }

  property("Add and Remove to Empty is Empty") = forAll { (element: A) =>
    val heap = insert(element, empty)
    isEmpty(deleteMin(heap))
  }

  property("sorted heap") = forAll { (h: H) =>
    def valueSmallerThanMinimumOnHeap(item: A, heap: H) = {
      if (isEmpty(heap)) true else (findMin(heap) >= item)
    }
    @tailrec
    def checkHeapIsOrdered(heap: H) : Boolean = {
      if (isEmpty(heap)) true
      else if (valueSmallerThanMinimumOnHeap(findMin(heap), deleteMin(heap))) checkHeapIsOrdered(deleteMin(heap))
      else false
    }
    checkHeapIsOrdered(h)
  }

  property("count check") = forAll { (h1: H, h2: H) =>
    def countItems(heap: H) = {
      @tailrec
      def innerCount(heap: H, counter: Int) : Int = {
        if (isEmpty(heap)) counter else innerCount(deleteMin(heap), counter+1)
      }
      innerCount(heap, 0)
    }
    countItems(meld(h1,h2)) == countItems(h1) + countItems(h2)
  }

  property("Minimum from each heaps is the same of the min of melded heap") = forAll { (heapOne: H, heapTwo: H) =>
    val mergedHeap = meld(heapOne, heapTwo)
    findMin(mergedHeap) == math.min(findMin(heapOne), findMin(heapTwo))
  }

  property("insert and delete, still min") = forAll { (h1: H) =>
    findMin(h1) == findMin(insert(Int.MaxValue, h1))
  }

  property("heaps are equal") = forAll { (heap1: H, heap2: H) =>
    @tailrec
    def allElementsEqual(heap1: H, heap2: H): Boolean = {
      if (isEmpty(heap1) && isEmpty(heap2))
        true
      else if (isEmpty(heap1) ^ isEmpty(heap2))
        false
      else if (findMin(heap1) == findMin(heap2))
        allElementsEqual(deleteMin(heap1), deleteMin(heap2))
      else false
    }
    allElementsEqual(meld(deleteMin(heap1), insert(findMin(heap1), heap2)), meld(heap1, heap2))
  }
}
