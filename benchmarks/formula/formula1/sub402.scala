import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub402 {
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
    (x) =>
      {
        val _4 = {
          def expr_eval: Expr => Int63 = (
            x =>
              x match {
                case NUM(x) => { x }
                case PLUS(x, y) => { expr_eval(x) + expr_eval(y) }
                case MINUS(x, y) => { expr_eval(x) - expr_eval(y) }
              }
          )
          x match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(x) => { not(eval(x)) }
            case ANDALSO(x, y) => { eval(x) && eval(y) }
            case ORELSE(x, y) => { eval(x) || eval(y) }
            case IMPLY(x, y) => { not(eval(x)) || eval(y) }
            case LESS(x, y) => { expr_eval(x) < expr_eval(y) }
          }
        }
    }
  )
}