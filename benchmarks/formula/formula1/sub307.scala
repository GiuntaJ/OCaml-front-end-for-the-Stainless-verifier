import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub307 {
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
  
  def eval_expr(e: Expr): Int63 = {
    e match {
      case NUM(x) => { x }
      case PLUS(x, y) => { eval_expr(x) + eval_expr(y) }
      case MINUS(x, y) => { eval_expr(x) - eval_expr(y) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { if (a == TRUE) false else true }
      case ANDALSO(x, y) => { if (x == TRUE && y == TRUE) true else false }
      case ORELSE(x, y) => { if (x == FALSE && y == FALSE) false else true }
      case IMPLY(x, y) => {
        
          if (
            x == FALSE && y == FALSE
          ) {
            true 
          } else if (
            x == FALSE && y == TRUE
          ) {
            true 
          } else if (
            x == TRUE && y == FALSE
          ) {
            false 
          } else {
            true
          }
      }
      case LESS(x, y) => { if (eval_expr(x) < eval_expr(y)) true else false }
    }
  }
  	  
}