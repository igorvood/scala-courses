package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)

    Signal[Set[Double]](
      if (delta() < 0) Set[Double]()
      else {
        Set[Double](
          (-b() + Math.sqrt(delta())) / (2 * a()),
          (-b() - Math.sqrt(delta())) / (2 * a())
        )
      }
    )


  }
}
