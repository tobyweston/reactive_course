package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property("delete min") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == a.max(b)
  }

  property("ordering") = forAll { (h1: H, h2: H) =>
    (elements(h1) ++ elements(h2)).sorted == elements(meld(h1, h2))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  def elements(heap: H): List[A] =
    if(isEmpty(heap)) Nil
    else findMin(heap) :: elements(deleteMin(heap))

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- frequency((1, value(empty)), (5, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
