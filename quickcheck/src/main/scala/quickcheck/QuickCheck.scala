package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  import scala.math._

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * Given
    */
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  property("del2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == max(a, b)
  }
  property("del3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == max(c, max(a, b))
  }

  property("meld1") = forAll { a: Int =>
    val h = insert(a, empty); isEmpty(deleteMin(meld(h, empty)))
  }
  property("meld2") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == min(findMin(a), findMin(b))
  }

  property("ordering") = forAll { a: H =>
    @tailrec
    def isSorted(min: Int, h: H): Boolean = {
      if (isEmpty(h)) true
      else if (min > findMin(h)) false
      else isSorted(findMin(h), deleteMin(h))
    }

    isSorted(findMin(a), deleteMin(a))
  }

}
