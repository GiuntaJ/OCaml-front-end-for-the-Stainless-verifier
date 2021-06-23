import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub48 {
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
  
  def eval_expr(expr_arg: Expr): Int63 = {
    expr_arg match {
      case NUM(n) => { n }
      case PLUS(expr1, expr2) => { eval_expr(expr1) + eval_expr(expr2) }
      case MINUS(expr1, expr2) => { eval_expr(expr1) - eval_expr(expr2) }
    }
  }
  
  def eval(fmula: Formula): Boolean = {
    fmula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f1) => { not(eval(f1)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(f1, f2) => { if (eval(f1)) eval(f2) else true }
      case LESS(expr1, expr2) => { eval_expr(expr1) < eval_expr(expr2) }
    }
  }
  
}
