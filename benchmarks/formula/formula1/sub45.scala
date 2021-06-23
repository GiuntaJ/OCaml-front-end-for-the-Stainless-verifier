import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub45 {
  /* let TRUE = true;; */
  /* let FALSE = false;; */
  
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
  
  def eval_expr(e: Expr): Int63 = {
    e match {
      case NUM(x) => { x }
      case PLUS(x, y) => { eval_expr(x) + eval_expr(y) }
      case MINUS(x, y) => { eval_expr(x) - eval_expr(y) }
    }
  }
  
  def eval_formula(p: Formula): Formula = {
    p match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(x) => { if (eval_formula(x) eq TRUE) FALSE else TRUE }
      case ANDALSO(x, y) => {
        if (eval_formula(x) eq TRUE && eval_formula(y) eq TRUE) TRUE else FALSE
      }
      case IMPLY(x, y) => {
        if (eval_formula(x) eq FALSE) TRUE else eval_formula(y)
      }
      case LESS(x, y) => { if (eval_expr(x) < eval_expr(y)) TRUE else FALSE }
    }
  }
  
  def eval(x: Formula): Boolean = { if (eval_formula(x) eq TRUE) true else false
  }
}