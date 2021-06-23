import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub53 {
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
  
  sealed case class Error(param0: String) extends Exception {}
  
  def val_and(f1: Boolean, f2: Boolean): Boolean = {
    (f1, f2) match {
      case (true, true) => { true }
      case _ => { false }
    }
  }
  
  def val_or(f1: Boolean, f2: Boolean): Boolean = {
    (f1, f2) match {
      case (false, false) => { false }
      case _ => { true }
    }
  }
  
  def val_imply(f1: Boolean, f2: Boolean): Boolean = {
    (f1, f2) match {
      case (true, false) => { false }
      case _ => { true }
    }
  }
  
  def val_not(f: Boolean): Boolean = {
    f match {
      case true => { false }
      case false => { true }
    }
  }
  
  
  def val_less[A](e1: A, e2: A): Boolean = { if (e1 < e2) true else false }
  
  def val_expr(e: Expr): Int63 = {
    e match {
      case NUM(k) => { k }
      case PLUS(e1, e2) => { val_expr(e1) + val_expr(e2) }
      case MINUS(e1, e2) => { val_expr(e1) - val_expr(e2) }
    }
  }
  
  
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { val_not(eval(f)) }
      case ANDALSO(f1, f2) => { val_and(eval(f1), eval(f2)) }
      case ORELSE(f1, f2) => { val_or(eval(f1), eval(f2)) }
      case IMPLY(f1, f2) => { val_imply(eval(f1), eval(f2)) }
      case LESS(e1, e2) => { val_less(val_expr(e1), val_expr(e2)) }
    }
  }
}
