import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub382 {
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
  
  def exprval(expr1: Expr): Int63 = {
    expr1 match {
      case NUM(n) => { n }
      case PLUS(expr1, expr2) => { exprval(expr1) + exprval(expr2) }
      case MINUS(expr1, expr2) => { exprval(expr1) - exprval(expr2) }
    }
  }
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(formula1) => { if (eval(formula1) eq true) false else true }
      case ANDALSO(formula1, formula2) => {
        if (eval(formula1) eq true && eval(formula2) eq true) true else false
      }
      case ORELSE(formula1, formula2) => {
        if (eval(formula1) eq true || eval(formula2) eq true) true else false
      }
      case IMPLY(formula1, formula2) => {
        if (eval(formula1) eq true && eval(formula2) eq false) false else true
      }
      case LESS(expr1, expr2) => {
        if (exprval(expr1) < exprval(expr2)) true else false
      }
    }
  }
}
