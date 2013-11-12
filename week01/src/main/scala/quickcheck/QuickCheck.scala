package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("insert and delete minimum") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("find minimum of two item heap") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = Math.min(a,b)

    findMin(h) == min
  }
  
  property("delete minimum of two item heap") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val maxHeap = insert(Math.max(a,b), empty)

    deleteMin(h) == maxHeap
  } 

  property("find minimum of melding of two heaps") = forAll { (h1: H, h2: H) =>
    val minMelded = findMin(meld(h1, h2))

    minMelded == findMin(h1) || minMelded == findMin(h2)
  }

  property("get a sorted sequence by recursively finding and deleting minimum") = forAll { (h1: H, h2: H) =>
    def listFromHeap(h: H, acc: List[A] = List.empty): List[A] = {
      if (isEmpty(h)) acc.reverse
      else listFromHeap(deleteMin(h), findMin(h) :: acc)
    }

    val allElements = listFromHeap(h1) ++ listFromHeap(h2)

    listFromHeap(meld(h1, h2)) == allElements.sorted
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
