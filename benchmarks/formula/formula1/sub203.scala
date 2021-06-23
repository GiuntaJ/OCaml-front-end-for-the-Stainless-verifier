import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub203 {
  /* KIHWAN KANG HW01-2 */
  
  /* PREDEFINED TYPES */
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
  /* END OF PREDEFINED TYPES */
  
  def eval(formula: Formula): Boolean = {
    val _2 = {
      def exprEval(expr) = {
        expr match {
          case NUM(a) => { a }
          case PLUS(a, b) => { exprEval(a) + exprEval(b) }
          case MINUS(a, b) => { exprEval(a) - exprEval(b) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(subformula) => {
          eval(subformula) match {
            case true => { false }
            case false => { true }
          }
        }
        case ANDALSO(a, b) => {
          (eval(a), eval(b)) match {
            case (true, true) => { true }
            case _ => { false }
          }
        }
        case ORELSE(a, b) => {
          (eval(a), eval(b)) match {
            case (false, false) => { false }
            case _ => { true }
          }
        }
        case IMPLY(a, b) => {
          (eval(a), eval(b)) match {
            case (true, false) => { false }
            case _ => { true }
          }
        }
        case LESS(a, b) => { if (exprEval(a) < exprEval(b)) true else false }
      }
    }
  }
}