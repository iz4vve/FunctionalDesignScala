package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val d = delta()
      if (d < 0) Set()
      else {
        val valueB = b()
        val valueA = a()

        Set(
          (-valueB + Math.sqrt(d)) / (2 * valueA),
          (-valueB - Math.sqrt(d)) / (2 * valueA)
        )
      }
    }
  }
}
