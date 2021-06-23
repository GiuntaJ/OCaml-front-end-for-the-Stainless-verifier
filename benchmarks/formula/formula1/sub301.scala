import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub301 {
  /* Department: Electrical and Computer Engineering */
  /* Student ID: 2010-11834 */
  /* Name: Kwonjoon Lee */
  /* Exercise #4 */
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
    val _2 = {
      def eval2(e: Expr): Int63 = {
        e match {
          case NUM(x) => { x }
          case PLUS(x, y) => { eval2(x) + eval2(y) }
          case MINUS(x, y) => { eval2(x) - eval2(y) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { if (x == TRUE) false else true }
        case ANDALSO(x, y) => { eval(x) && eval(y) }
        case ORELSE(x, y) => { eval(x) || eval(y) }
        case IMPLY(x, y) => { if (x == TRUE && y == FALSE) false else true }
        case LESS(x, y) => { if (eval2(x) < eval2(y)) true else false }
      }
    }
  }
  	
}