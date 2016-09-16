package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    val va: Double = a()
    val vb: Double = b()
    val vc: Double = c()

    // delta = b^2 - 4ac
    Signal(vb * vb - 4.0 * va * vc)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val vd = delta()
    val va = a()
    val vb = b()
    val va2 = 2*va

    Signal(Math.sqrt(vd) match {
        case dsqrt if dsqrt.isNaN => Set()
        case dsqrt if dsqrt == 0 => Set(-vb / va2)
        case dsqrt => Set((-vb - dsqrt)/va2, (-vb + dsqrt)/va2)
      })
  }
}
