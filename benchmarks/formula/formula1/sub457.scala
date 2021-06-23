import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub457 {
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
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def expr_int: Expr => Int63 = (
            (g) =>
              {
                g match {
                  case NUM(x) => { x }
                  case PLUS(x, y) => { expr_int(x) + expr_int(y) }
                  case MINUS(x, y) => { expr_int(x) - expr_int(y) }
                }
            }
          )
          f match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(x) => { if (eval(x)) false else true }
            case ANDALSO(x, y) => { if (eval(x) && eval(y)) true else false }
            case ORELSE(x, y) => { if (eval(x) || eval(y)) true else false }
            case IMPLY(x, y) => { if (eval(x) && eval(NOT(y))) false else true }
            case LESS(a, b) => { expr_int(a) < expr_int(b) }
          }
        }
    }
  )
}