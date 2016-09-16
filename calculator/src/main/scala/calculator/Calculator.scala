package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions map { case (str, expr) => (str, Signal(eval(expr(), namedExpressions))) }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def math(op: (Double, Double) => Double)(left: Expr, right: Expr) = op(eval(left, references), eval(right, references))
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references - name)
      case Plus(a, b) => math(_ + _)(a,b)
      case Minus(a, b) => math(_ - _)(a,b)
      case Times(a, b) => math(_ * _)(a,b)
      case Divide(a, b) => math(_ / _)(a,b)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
