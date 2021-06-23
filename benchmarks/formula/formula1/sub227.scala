import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub227 {
  /* ex2 */
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
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      def eval_expr(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(a, b) => { eval_expr(a) + eval_expr(b) }
          case MINUS(a, b) => { eval_expr(a) - eval_expr(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { not(eval(x)) }
        case ANDALSO(l, r) => { eval(l) && eval(r) }
        case ORELSE(l, r) => { eval(l) || eval(r) }
        case IMPLY(l, r) => { not(eval(l)) || eval(l) && eval(r) }
        case LESS(l, r) => { eval_expr(l) < eval_expr(r) }
      }
    }
  }
}