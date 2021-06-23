import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub504 {
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
      def calc(expr) = {
        expr match {
          case NUM(n) => { n }
          case PLUS(expr1, expr2) => { calc(expr1) + calc(expr2) }
          case MINUS(expr1, expr2) => { calc(expr1) - calc(expr2) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(fm) => { not(eval(fm)) }
        case ANDALSO(fm1, fm2) => { eval(fm1) && eval(fm2) }
        case ORELSE(fm1, fm2) => { eval(fm1) || eval(fm2) }
        case IMPLY(fm1, fm2) => { not(eval(fm1)) || eval(fm2) }
        case LESS(expr1, expr2) => { calc(expr1) < calc(expr2) }
      }
    }
  }
}