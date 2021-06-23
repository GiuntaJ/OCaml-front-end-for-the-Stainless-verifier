import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub481 {
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
  
  def evalExpr(ex: Expr): Int63 = {
    ex match {
      case NUM(i) => { i }
      case PLUS(e1, e2) => { evalExpr(e1) + evalExpr(e2) }
      case MINUS(e1, e2) => { evalExpr(e1) - evalExpr(e2) }
    }
  }
  
  def evalForm(fm: Formula): Formula = {
    fm match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(f1) => { if (evalForm(f1) == TRUE) FALSE else TRUE }
      case ANDALSO(f1, f2) => {
        if (evalForm(f1) == TRUE && evalForm(f2) == TRUE) TRUE else FALSE
      }
      case ORELSE(f1, f2) => {
        if (evalForm(f1) == TRUE || evalForm(f2) == TRUE) TRUE else FALSE
      }
      case IMPLY(f1, f2) => {
        if (evalForm(f1) == TRUE && evalForm(f2) == FALSE) FALSE else TRUE
      }
      case LESS(e1, e2) => { if (evalExpr(e1) < evalExpr(e2)) TRUE else FALSE }
    }
  }
  
  
  def eval(fm: Formula): Boolean = {
    evalForm(fm) match {
      case TRUE => { true }
      case FALSE => { false }
      case _ => { false }
    }
  }
}