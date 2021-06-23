import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub78 {
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
  def cal(z) = {
    z match {
      case NUM(x) => { x }
      case PLUS(x, y) => { cal(x) + cal(y) }
      case MINUS(x, y) => { cal(x) - cal(y) }
    }
  }
  
  def eval(formula) = {
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case ORELSE(FALSE, FALSE) => { false }
      case ORELSE(TRUE, _) | ORELSE(_, TRUE) => { true }
      case IMPLY(TRUE, FALSE) => { false }
      case IMPLY(_, TRUE) | IMPLY(FALSE, FALSE) => { true }
      case NOT(a) => { if (eval(a) == true) false else true }
      case ANDALSO(a, b) => {
        if (eval(a) == true && eval(b) == true) true else false
      }
      case ORELSE(a, b) => {
        if (eval(a) == false && eval(b) == false) false else true
      }
      case IMPLY(a, b) => {
        if (eval(a) == true && eval(b) == false) false else true
      }
      case LESS(a, b) => { cal(a) < cal(b) }
    }
  }
}