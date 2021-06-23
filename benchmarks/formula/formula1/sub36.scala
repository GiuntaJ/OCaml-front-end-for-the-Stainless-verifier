import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub36 {
  
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
  def expreval(exp: Expr): Int63 = {
    exp match {
      case NUM(n) => { n }
      case PLUS(n1, n2) => { expreval(n1) + expreval(n2) }
      case MINUS(n1, n2) => { expreval(n1) - expreval(n2) }
    }
  }
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { if (eval(f)) false else true }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(f1, f2) => { if (eval(f1) && eval(f2) == false) false else true
      }
      case LESS(e1, e2) => { expreval(e1) < expreval(e2) }
    }
  }
}
