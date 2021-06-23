import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub11 {
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
  
  def eval(formulain: Formula): Boolean = {
    val _2 = {
      def calc(exprin) = {
        exprin match {
          case NUM(x) => { x }
          case PLUS(x, y) => { calc(x) + calc(y) }
          case MINUS(x, y) => { calc(x) - calc(y) }
        }
      }
      formulain match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(interform) => { not(eval(interform)) }
        case ANDALSO(interform1, interform2) => {
          eval(interform1) && eval(interform2)
        }
        case ORELSE(interform1, interform2) => {
          eval(interform1) || eval(interform2)
        }
        case IMPLY(interform1, interform2) => {
          not(eval(interform1) && not(eval(interform2)))
        }
        case LESS(expr1, expr2) => { calc(expr1) < calc(expr2) }
      }
    }
  }
}