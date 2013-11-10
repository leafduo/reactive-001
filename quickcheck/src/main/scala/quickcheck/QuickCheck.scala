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
  
  // Let's implment 4 hints
  property("getMin") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }
  
  property("giveMeEmpty") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }
  
  property("ordering") = forAll { (h: H) =>
    def isOrdered(h: H): Boolean =
      if (isEmpty(h)) 
        true
      else {
        val min = findMin(h)
        val h2 = deleteMin(h)
        (isEmpty(h2) || min <= findMin(h2)) && isOrdered(h2)
      }
    isOrdered(h)
  }
  
  property("meldMin") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }
  
  // The bad cases involves meld and links, so I tried...
  property("meldTheSame") = forAll { (h1: H, h2: H, n: Int) =>
    def isHeapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2))
        true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && isHeapEqual(deleteMin(h1), deleteMin(h2))
      }
    isHeapEqual(meld(insert(n, h1), h2),
              meld(h1, insert(n, h2)))
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
