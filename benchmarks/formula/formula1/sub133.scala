import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub133 {
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
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(formula) => { not(eval(formula)) }
      case ANDALSO(formula1, formula2) => { eval(formula1) && eval(formula2) }
      case ORELSE(formula1, formula2) => { eval(formula1) || eval(formula2) }
      case IMPLY(formula1, formula2) => { not(eval(formula1)) || eval(formula2)
      }
      case LESS(expr1, expr2) => {
        val _2 = {
          def value(expr) = {
            expr match {
              case NUM(n) => { n }
              case PLUS(expr1, expr2) => { value(expr1) + value(expr2) }
              case MINUS(expr1, expr2) => { value(expr1) - value(expr2) }
            }
          }
          value(expr1) < value(expr2)
        }
      }
    }
  }
  
  		
}