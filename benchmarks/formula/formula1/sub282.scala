import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub282 {
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def evalexpr(e: Expr): Int63 = {
    e match {
      case NUM(t) => { t }
      case PLUS(e1, e2) => { evalexpr(e1) + evalexpr(e2) }
      case MINUS(e1, e2) => { evalexpr(e1) - evalexpr(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(tail) => { if (eval(tail) eq true) false else true }
      case ANDALSO(t1, t2) => { if (eval(t1) && eval(t2)) true else false }
      case ORELSE(t1, t2) => { if (eval(t1) || eval(t2)) true else false }
      case IMPLY(t1, t2) => { if (eval(t1) && not(eval(t2))) false else true }
      case LESS(e1, e2) => { evalexpr(e1) < evalexpr(e2) }
    }
  }
}