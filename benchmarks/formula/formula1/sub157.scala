import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub157 {
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
  	/* make eval : formula -> bool*/
  def eval(formula: Formula): Boolean = {
    val _2 = {
      def eval_expr(expr) = {
        expr match {
          case NUM(i) => { i }
          case PLUS(a, b) => { eval_expr(a) + eval_expr(b) }
          case MINUS(a, b) => { eval_expr(a) - eval_expr(b) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(fm) => { not(eval(fm)) }
        case ANDALSO(fm1, fm2) => { eval(fm1) && eval(fm2) }
        case ORELSE(fm1, fm2) => { eval(fm1) || eval(fm2) }
        case IMPLY(fm1, fm2) => { not(eval(fm1)) || eval(fm2) }
        case LESS(exp1, exp2) => {
          if (eval_expr(exp1) < eval_expr(exp2)) true else false
        }
      }
    }
  }
}
