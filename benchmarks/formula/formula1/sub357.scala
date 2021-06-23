import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub357 {
  /*
    CSE/2015-21233/김종권
    Homework 1-4
  */
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
  
  def eval_expr(expr: Expr): Int63 = {
    expr match {
      case NUM(i) => { i }
      case PLUS(e1, e2) => { eval_expr(e1) + eval_expr(e2) }
      case MINUS(e1, e2) => { eval_expr(e1) - eval_expr(e2) }
    }
  }
                       
  def eval_0(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval_0(f)) }
      case ANDALSO(f1, f2) => { eval_0(f1) && eval_0(f2) }
      case ORELSE(f1, f2) => { eval_0(f1) || eval_0(f2) }
      case IMPLY(f1, f2) => { not(eval_0(f1)) || eval_0(f2) }
      case LESS(e1, e2) => { eval_expr(e1) < eval_expr(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = { eval_0(f) }
}