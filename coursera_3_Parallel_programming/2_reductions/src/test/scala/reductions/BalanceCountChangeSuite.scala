package reductions

import java.util.concurrent.*
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory
import scala.collection.*

class BalanceCountChangeSuite extends munit.FunSuite {

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(ParallelParenthesesBalancing.balance("(if (zero? x) max (/ 1 x))".toArray))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(ParallelParenthesesBalancing.balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!ParallelParenthesesBalancing.balance(":-)".toArray))
  }

  test("balance: counting is not enough") {
    assert(!ParallelParenthesesBalancing.balance("())(".toArray))
  }

  test("balance: counting is not enough 2") {
    assert(!ParallelParenthesesBalancing.balance("()((()(".toArray))
  }

  test("balance: counting is not enough 3") {
    assert(!ParallelParenthesesBalancing.balance("()))()(".toArray))
  }

  test("balance: test") {
    assert(ParallelParenthesesBalancing.balance("(if (zero? x) max (/ 1 x))\n  I told him (that it's not (yet) done). (But he wasn't listening)".toArray))
  }

  test("not balance: test") {
    assert(!ParallelParenthesesBalancing.balance("(o_()\n:-)\n())(".toArray))
  }
}
