import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub92 {
  /* 2009-13384, CHO Hyunik */
  
  
  
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
  
  
  
  def eval(formula1: Formula): Boolean = {
    val _2 = {
      def exprCalculation(expr1) = {
        expr1 match {
          case NUM(a) => { a }
          case PLUS(a, b) => { exprCalculation(a) + exprCalculation(b) }
          case MINUS(a, b) => { exprCalculation(a) - exprCalculation(b) }
        }
      }
      formula1 match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { not(eval(a)) }
        case ANDALSO(a, b) => { eval(a) && eval(b) }
        case ORELSE(a, b) => { eval(a) || eval(b) }
        case IMPLY(a, b) => { eval(b) || not(eval(a)) }
        case LESS(a, b) => { exprCalculation(a) < exprCalculation(b) }
      }
    }
  }
}