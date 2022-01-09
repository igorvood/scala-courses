package quickcheck

//import common._

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    val value: Gen[H] = for {
      a: Int <- arbitrary[Int]
      h <- oneOf[H](empty, genHeap)
    } yield insert(a, h)
    value
    //    ???
  }

  lazy val genHeapMore: Gen[H] = {
    val value: Gen[H] = for {
      a: Int <- arbitrary[Int]
      h <- Gen.frequency(
        (1, empty),
        (10, genHeap)
      )
    } yield insert(a, h)
    value
   //    ???
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minStepByStep") = forAll { (h: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }

    val xs = remMin(h, Nil)
    xs == xs.sorted
  }

  property("minOnOfHeap") = forAll { (h: H, h1: H) =>
    val a = findMin(h)
    val a1 = findMin(h1)
    val merged = meld(h, h1)
    val minMerg = findMin(merged)
    minMerg == a || minMerg == a1
  }


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insTwoInEmptyAndReturnMin") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a1, empty)
    val h1 = insert(a2, h)
    val i = if (a1 < a2) a1 else a2
    findMin(h1) == i
  }

  property("insInEmpty") = forAll { (a1: Int) =>
    val h = insert(a1, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  property("testLink") = forAll { (a1: H, b: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }

    val value: List[Int] = remMin(a1, Nil)
    val value1: List[Int] = remMin(meld(a1, b), Nil)

    val value2 = value.to(LazyList)
      .withFilter(p => !value1.contains(p))
      .map(p => p)
      .take(0)


    value2.isEmpty
  }

  //  trait Bogus3BinomialHeap extends BinomialHeap {
  //    override protected def link(t1: Node, t2: Node): Node = // t1.r == t2.r
  //      if (ord.lteq(t1.x, t2.x)) Node(t1.x, t1.r + 1, t1 :: t1.c) else Node(t2.x, t2.r + 1, t2 :: t2.c)
  //    //  if (ord.lteq(t1.x, t2.x)) Node(t1.x, t1.r + 1, t2 :: t1.c) else Node(t2.x, t2.r + 1, t1 :: t2.c)
  //    //  case class Node(x: A, r: Rank, c: List[Node])
  //  }

  property("meldMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }

    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }


}
