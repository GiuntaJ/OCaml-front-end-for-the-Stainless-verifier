import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub395 {
  
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
  
  def evalexpr(x: Expr): Int63 = {
    x match {
      case NUM(y) => { y }
      case PLUS(a, b) => { evalexpr(a) + evalexpr(b) }
      case MINUS(a, b) => { evalexpr(a) - evalexpr(b) }
    }
  }
  
  def eval(a: Formula): Boolean = {
    a match {
      case FALSE => { false }
      case TRUE => { true }
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => { eval(x) && eval(y) }
      case ORELSE(x, y) => { eval(x) || eval(y) }
      case IMPLY(x, y) => { not(eval(x)) || eval(y) }
      case LESS(x, y) => { evalexpr(x) < evalexpr(y) }
    }
  }
}
