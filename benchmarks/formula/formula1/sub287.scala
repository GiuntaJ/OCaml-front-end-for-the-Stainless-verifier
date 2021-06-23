import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub287 {
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
  
  def evalexpr(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(e0, e1) => { evalexpr(e0) + evalexpr(e1) }
      case MINUS(e0, e1) => { evalexpr(e0) - evalexpr(e1) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f0) => { not(eval(f0)) }
      case ANDALSO(f0, f1) => { eval(f0) && eval(f1) }
      case ORELSE(f0, f1) => { eval(f0) || eval(f1) }
      case IMPLY(f0, f1) => { not(eval(f0)) || eval(f0) && eval(f1) }
      case LESS(e0, e1) => { evalexpr(e0) < evalexpr(e1) }
    }
  }
}
