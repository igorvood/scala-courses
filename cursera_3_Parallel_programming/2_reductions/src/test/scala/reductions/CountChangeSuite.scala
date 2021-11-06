package reductions

import java.util.concurrent.*
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory
import scala.collection.*

class CountChangeSuite extends munit.FunSuite {

  test("countChange example given in instructions") {
    assert(ParallelCountChange.countChange(4, List(1, 2)) == 3)
  }

  test("countChange: sorted CHF") {
    assert(ParallelCountChange.countChange(300, List(5, 10, 20, 50, 100, 200, 500)) == 1022)
  }

  test("countChange: no pennies") {
    assert(ParallelCountChange.countChange(301, List(5, 10, 20, 50, 100, 200, 500)) == 0)
  }

  test("countChange: unsorted CHF") {
    assert(ParallelCountChange.countChange(300, List(500, 5, 50, 100, 20, 200, 10)) == 1022)
  }

  test("countChange: No money") {
    assert(ParallelCountChange.countChange(0, List(500, 5, 50, 100, 20, 200, 10)) == 1)
  }

  test("countChange: No coins") {
    assert(ParallelCountChange.countChange(20, List()) == 0)
  }
}
