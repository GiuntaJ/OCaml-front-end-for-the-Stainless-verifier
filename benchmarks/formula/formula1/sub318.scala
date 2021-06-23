import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub318 {
  /* define the type */
  
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
  
  	
  			
  /* define the operator */
  
  def expr_to_int(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(e1, e2) => { expr_to_int(e1) + expr_to_int(e2) }
      case MINUS(e1, e2) => { expr_to_int(e1) - expr_to_int(e2) }
    }
  }
  
  def less_cmd(((e1, e2))) = {
    if (expr_to_int(e1) < expr_to_int(e2)) TRUE else FALSE
  }
  		
  def and_also_cmd(((f1, f2))) = {
    (f1, f2) match {
      case (TRUE, TRUE) => { TRUE }
      case _ => { FALSE }
    }
  }
  
  def or_else_cmd(((f1, f2))) = {
    (f1, f2) match {
      case (FALSE, FALSE) => { FALSE }
      case _ => { TRUE }
    }
  }
  		
  def imply_cmd(((f1, f2))) = {
    (f1, f2) match {
      case (TRUE, FALSE) => { FALSE }
      case _ => { TRUE }
    }
  }
  
  def not_cmd(f: Formula): Formula = {
    f match {
      case TRUE => { FALSE }
      case FALSE => { TRUE }
      case NOT(innerF) => { innerF }
      case ANDALSO(f1, f2) => { not_cmd(and_also_cmd(f1, f2)) }
      case ORELSE(f1, f2) => { not_cmd(or_else_cmd(f1, f2)) }
      case IMPLY(f1, f2) => { not_cmd(imply_cmd(f1, f2)) }
      case LESS(e1, e2) => { not_cmd(less_cmd(e1, e2)) }
    }
  }
  
  
  def formal_to_bool(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(innerF) => { formal_to_bool(not_cmd(f)) }
      case ANDALSO(f1, f2) => { formal_to_bool(and_also_cmd(f1, f2)) }
      case ORELSE(f1, f2) => { formal_to_bool(or_else_cmd(f1, f2)) }
      case IMPLY(f1, f2) => { formal_to_bool(imply_cmd(f1, f2)) }
      case LESS(e1, e2) => { formal_to_bool(less_cmd(e1, e2)) }
    }
  }
  
  
  
  /* Define the function eval */
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(innerF) => { formal_to_bool(not_cmd(innerF)) }
      case ANDALSO(f1, f2) => { formal_to_bool(and_also_cmd(f1, f2)) }
      case ORELSE(f1, f2) => { formal_to_bool(or_else_cmd(f1, f2)) }
      case IMPLY(f1, f2) => { formal_to_bool(imply_cmd(f1, f2)) }
      case LESS(e1, e2) => { formal_to_bool(less_cmd(e1, e2)) }
    }
  }
  
  
  	
}