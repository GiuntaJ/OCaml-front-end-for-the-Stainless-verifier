import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub489 {
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
          
  def inteval(x: Expr): Int63 = {
    x match {
      case NUM(a) => { a }
      case PLUS(a, b) => { inteval(a) + inteval(b) }
      case MINUS(a, b) => { inteval(a) - inteval(b) }
    }
  }
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def eval(x: Formula): Boolean = {
    x match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { if (eval(a)) false else true }
      case ANDALSO(a, b) => { if (eval(a) && eval(b)) true else false }
      case ORELSE(a, b) => { if (not(eval(a)) && not(eval(b))) false else true }
      case IMPLY(a, b) => { if (eval(a) && not(eval(b))) false else true }
      case LESS(a, b) => { inteval(a) < inteval(b) }
    }
  } 
}