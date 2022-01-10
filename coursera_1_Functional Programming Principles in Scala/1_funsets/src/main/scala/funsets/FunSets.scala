package funsets

import scala.collection.immutable.HashSet
import scala.collection.{immutable, mutable}

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /*  class DataInt /*(val elem: Int)*/ extends FunSet {

      val setInt: immutable.HashSet[Int] = new HashSet[Int]()

      override def apply(v1: Int): Boolean = if (v1 > 0) true else false

      def addInt(elem: Int): Unit = {
        if (apply(elem)) {
          setInt.incl(elem)
          //        setInt.add(elem)
        }
      }
    }*/

  //  val intToBoolean1: Int => Boolean = (elem: Int) => if (elem > 0) true else false
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = new FunSet {
    override def apply(v1: Int): Boolean = {
      if (v1 == elem) true else false
    }
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = new FunSet {

    override def apply(elem: Int): Boolean = {
      contains(s, elem) || contains(t, elem)
    }
  }


  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (elem: Int) => {
    contains(s, elem) && contains(t, elem)
  }


  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = new FunSet {
    override def apply(elem: Int): Boolean = {
      math.Ordering
      contains(s, elem) && !contains(t, elem)
    }
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = new FunSet {
    override def apply(v1: Int): Boolean = {
      p.apply(v1) && s.apply(v1)
    }
  }


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > -bound && a < bound) false
      else if (p(a)) contains(s, a)
      else iter(a + 1)
    }

    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    forall(s, p) || forall(p, s)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = {
    new FunSet {
      override def apply(v1: Int): Boolean = {
        s(f(v1))
      }
    }
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets {
  val set: mutable.HashSet[Int] = mutable.HashSet.empty
}