import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub249 {
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
      def eval_expr(e: Expr): Int63 = {
        e match {
          case NUM(n) => { n }
          case PLUS(n1, n2) => { eval_expr(n1) + eval_expr(n2) }
          case MINUS(n1, n2) => { eval_expr(n1) - eval_expr(n2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(b) => { not(eval(b)) }
        case ANDALSO(b1, b2) => { eval(b1) && eval(b2) }
        case ORELSE(b1, b2) => { eval(b1) || eval(b2) }
        case IMPLY(b1, b2) => { not(eval(b1)) || eval(b2) }
        case LESS(n1, n2) => { eval_expr(n1) < eval_expr(n2) }
      }
    }
  }
}