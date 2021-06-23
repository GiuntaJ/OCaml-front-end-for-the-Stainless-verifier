import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub350 {
  /* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
   * Programming Languages 2015 Fall
   * Homework 1, Exercise 4 */
  
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
  
  def evalExpr(expr: Expr): Int63 = {
    expr match {
      case NUM(i) => { i }
      case PLUS(i1, i2) => { evalExpr(i1) + evalExpr(i2) }
      case MINUS(i1, i2) => { evalExpr(i1) - evalExpr(i2) }
    }
  }
  
  def eval(formula: Formula): Boolean = {
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(p, q) => { not(eval(p)) || eval(q) }
      case LESS(e1, e2) => { evalExpr(e1) < evalExpr(e2) }
    }
  }
}