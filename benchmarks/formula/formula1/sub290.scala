import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub290 {
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
  
  /* evaluate expression */
  def eval_expr(expr: Expr): Int63 = {
    expr match {
      case NUM(e) => { e }
      case PLUS(e1, e2) => { eval_expr(e1) + eval_expr(e2) }
      case MINUS(e1, e2) => { eval_expr(e1) - eval_expr(e2) }
    }
  }
  
  /* evaluate formula */
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f1) => { if (eval(f1)) false else true }
      case ANDALSO(f1, f2) => { if (eval(f1) && eval(f2)) true else false }
      case ORELSE(f1, f2) => { if (eval(f1) || eval(f2)) true else false }
      case IMPLY(f1, f2) => { if (not(eval(f1)) || eval(f2)) true else false }
      case LESS(e1, e2) => { if (eval_expr(e1) < eval_expr(e2)) true else false
      }
    }
  }
}