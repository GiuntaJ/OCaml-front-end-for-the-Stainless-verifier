import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub344 {
  /*
      PL 1-4
      2008-11609 박성원
  */
  
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
      def eval_expr(exp) = {
        exp match {
          case NUM(n) => { n }
          case PLUS(n, m) => { eval_expr(n) + eval_expr(m) }
          case MINUS(n, m) => { eval_expr(n) - eval_expr(m) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { not(eval(x)) }
        case ANDALSO(x, y) => { eval(x) && eval(y) }
        case ORELSE(x, y) => { eval(x) || eval(y) }
        case IMPLY(x, y) => { not(eval(x)) || eval(y) }
        case LESS(e1, e2) => { eval_expr(e1) < eval_expr(e2) }
      }
    }
  }
}