package crs

import scalaz.{Functor, Free, Cofree}
import matryoshka.data.Fix
import matryoshka._
import matryoshka.implicits._

sealed abstract class ExprF[A] extends Product with Serializable

object ExprF {
  case class LiteralF[A](value: Int) extends ExprF[A]
  case class AddF[A](lhs: A, rhs: A) extends ExprF[A]
  case class MultiplyF[A](lhs: A, rhs: A) extends ExprF[A]
  case class SubtractF[A](lhs: A, rhs: A) extends ExprF[A]

  implicit def functorExprF: Functor[ExprF] = new Functor[ExprF] {
    def map[A, B](e: ExprF[A])(f: A => B): ExprF[B] = e match {
      case LiteralF(l) => (LiteralF(l): ExprF[B])
      case AddF(l, r) => AddF(f(l), f(r))
      case MultiplyF(l, r) => MultiplyF(f(l), f(r))
      case SubtractF(l, r) => SubtractF(f(l), f(r))
    }
  }
}

object Expr {
  import ExprF._

  type Expr = Fix[ExprF]
  type FreeExpr = Free[ExprF, Unit]
  type CofreeExpr = Cofree[ExprF, Unit]

  def Literal(v: Int): Expr = Fix(LiteralF(v))
  def Add(l: Expr, r: Expr): Expr = Fix(AddF(l, r))
  def Multiply(l: Expr, r: Expr): Expr = Fix(MultiplyF(l, r))
  def Subtract(l: Expr, r: Expr): Expr = Fix(SubtractF(l, r))

  def evaluateƒ: Algebra[ExprF, Int] = {
    case LiteralF(i)     => i
    case AddF(l, r)      => l + r
    case MultiplyF(l, r) => l * r
    case SubtractF(l, r) => l - r
  }

  def convertƒ: Algebra[ExprF, Expr] = {
    case MultiplyF(l, r) => Fix(AddF(l, r))
    case otherwise       => Fix(otherwise)
  }

  def evaluate(e: Expr)(implicit R: Recursive.Aux[Expr, ExprF]): Int =
    R.cata[Int](e)(evaluateƒ)

  def evaluate0(e: Expr): Int =
    e.cata(evaluateƒ)
}
