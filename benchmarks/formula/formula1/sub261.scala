import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub261 {
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
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { if (eval(p)) false else true }
      case ANDALSO(p, q) => { if (eval(p)) eval(q) else false }
      case ORELSE(p, q) => { if (eval(p)) true else eval(q) }
      case IMPLY(p, q) => { if (eval(p)) eval(q) else true }
      case LESS(x, y) => {
        val _2 = {
          def expr_eval(e: Expr): Int63 = {
            e match {
              case NUM(n) => { n }
              case PLUS(e1, e2) => { expr_eval(e1) + expr_eval(e2) }
              case MINUS(e1, e2) => { expr_eval(e1) - expr_eval(e2) }
            }
          }
          expr_eval(x) < expr_eval(y)
        }
      }
    }
  }
}