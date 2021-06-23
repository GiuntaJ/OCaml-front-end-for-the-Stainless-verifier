import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub73 {
  /* PL HW1-5 "참거짓"
     2007-11738
     알렉산더 */
  
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
  
  /* eval: formula -> bool */
  def eval(f: Formula): Boolean = {
    val _2 = {
      def evalExp(exp) = {
        exp match {
          case NUM(num) => { num }
          case PLUS(exp1, exp2) => { evalExp(exp1) + evalExp(exp2) }
          case MINUS(exp1, exp2) => { evalExp(exp1) - evalExp(exp2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f1) => { not(eval(f1)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => {
          (eval(f1), eval(f2)) match {
            case (true, true) => { true }
            case (true, false) => { false }
            case (false, true) => { true }
            case (false, false) => { true }
          }
        }
        case LESS(exp1, exp2) => {
          if (evalExp(exp1) < evalExp(exp2)) true else false
        }
      }
    }
  }
}