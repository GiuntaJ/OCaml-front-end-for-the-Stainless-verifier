import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub199 {
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  def eval_expr(n: Expr): Int63 = {
    n match {
      case NUM(a) => { a }
      case PLUS(left, right) => { eval_expr(left) + eval_expr(right) }
      case MINUS(left, right) => { eval_expr(left) - eval_expr(right) }
    }
  }
  def eval(m: Formula): Boolean = {
    m match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(left, right) => { eval(left) && eval(right) }
      case ORELSE(left, right) => { eval(left) || eval(right) }
      case IMPLY(left, right) => { not(eval(left)) || eval(right) }
      case LESS(left, right) => { eval_expr(left) < eval_expr(right) }
    }
  }
}