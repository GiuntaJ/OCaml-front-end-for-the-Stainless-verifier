import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub71 {
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
  
  def cal(expr: Expr): Int63 = {
    expr match {
      case NUM(x) => { x }
      case PLUS(x, y) => { cal(x) + cal(y) }
      case MINUS(x, y) => { cal(x) - cal(y) }
    }
  }
  
  def eval(formula: Formula): Boolean = {
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => {
        x match {
          case TRUE => { false }
          case FALSE => { true }
          case _ => { eval(x) }
        }
      }
      case ANDALSO(x, y) => {
        
          if (
            x ne TRUE || x ne FALSE
          ) {
            eval(x) 
          } else if (
            y ne TRUE || y ne FALSE
          ) {
            eval(y) 
          } else {
            (x, y) match {
              case (TRUE, TRUE) => { true }
              case (_, _) => { false }
            }
          }
      }
      case ORELSE(x, y) => {
        
          if (
            x ne TRUE || x ne FALSE
          ) {
            eval(x) 
          } else if (
            y ne TRUE || y ne FALSE
          ) {
            eval(y) 
          } else {
            (x, y) match {
              case (FALSE, FALSE) => { false }
              case (_, _) => { true }
            }
          }
      }
      case IMPLY(x, y) => {
        
          if (
            x ne TRUE || x ne FALSE
          ) {
            eval(x) 
          } else if (
            y ne TRUE || y ne FALSE
          ) {
            eval(y) 
          } else if (
            x == FALSE
          ) {
            true 
          } else if (
            y == TRUE
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(e1, e2) => { if (cal(e1) < cal(e2)) true else false }
    }
  }
}