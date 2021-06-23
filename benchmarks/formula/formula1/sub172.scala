import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub172 {
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
  
  def evalexpr(ex: Expr): Int63 = {
    ex match {
      case NUM(i) => { i }
      case PLUS(e1, e2) => { evalexpr(e1) + evalexpr(e2) }
      case MINUS(e1, e2) => { evalexpr(e1) - evalexpr(e2) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(f, g) => { eval(f) && eval(g) }
      case ORELSE(f, g) => { eval(f) || eval(g) }
      case IMPLY(f, g) => { not(eval(f)) || eval(g) }
      case LESS(ex1, ex2) => { evalexpr(ex1) < evalexpr(ex2) }
    }
  }
}
