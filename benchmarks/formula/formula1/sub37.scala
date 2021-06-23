import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub37 {
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
  
  def eval(t: Formula): Boolean = {
    val _2 = {
      def inteval(x) = {
        x match {
          case PLUS(x1, x2) => { inteval(x1) + inteval(x2) }
          case MINUS(x1, x2) => { inteval(x1) - inteval(x2) }
          case NUM(x) => { x }
        }
      }
      t match {
        case NOT(t) => { not(eval(t)) }
        case ANDALSO(t1, t2) => { eval(t1) && eval(t2) }
        case ORELSE(t1, t2) => { eval(t1) || eval(t2) }
        case IMPLY(t1, t2) => { not(eval(t1)) || eval(t2) }
        case LESS(t1, t2) => { inteval(t1) < inteval(t2) }
        case TRUE => { true }
        case FALSE => { false }
      }
    }
  }
}