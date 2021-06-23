import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub192 {
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
  
  def eval(formula: Formula): Boolean = {
    val _2 = {
      def eval_expr(expr) = {
        expr match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { eval_expr(e1) + eval_expr(e2) }
          case MINUS(e1, e2) => { eval_expr(e1) - eval_expr(e2) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { not(eval(f)) }
        case ANDALSO(f1, f2) => { if (eval(f1)) eval(f2) else false }
        case ORELSE(f1, f2) => { if (eval(f1)) true else eval(f2) }
        case IMPLY(f1, f2) => { if (eval(f1)) eval(f2) else true }
        case LESS(e1, e2) => { eval_expr(e1) < eval_expr(e2) }
      }
    }
  }
}