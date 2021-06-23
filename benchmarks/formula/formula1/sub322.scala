import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub322 {
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
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { if (eval(a)) false else true }
      case ANDALSO(a, b) => { if (eval(a) && eval(b)) true else false }
      case ORELSE(a, b) => { if (eval(a) || eval(b)) true else false }
      case IMPLY(a, b) => { if (eval(a) && not(eval(b))) false else true }
      case LESS(a, b) => {
        val _2 = {
          def eeval(e) = {
            e match {
              case NUM(i) => { i }
              case PLUS(a, b) => { eeval(a) + eeval(b) }
              case MINUS(a, b) => { eeval(a) - eeval(b) }
            }
          }
          if (eeval(a) < eeval(b)) true else false
        }
      }
    }
  }
}
