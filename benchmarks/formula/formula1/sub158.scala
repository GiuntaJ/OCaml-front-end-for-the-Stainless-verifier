import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub158 {
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
      def calc_expr(expr) = {
        expr match {
          case NUM(x) => { x }
          case PLUS(x, y) => { calc_expr(x) + calc_expr(y) }
          case MINUS(x, y) => { calc_expr(x) - calc_expr(y) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { if (eval(x) == true) false else true }
        case ANDALSO(x, y) => { eval(x) && eval(y) }
        case ORELSE(x, y) => { eval(x) || eval(y) }
        case IMPLY(x, y) => {
          
            if (
              eval(x) == false
            ) {
              true 
            } else if (
              eval(y) == true
            ) {
              true 
            } else {
              false
            }
        }
        case LESS(x, y) => { if (calc_expr(x) < calc_expr(y)) true else false }
      }
    }
  }
}
