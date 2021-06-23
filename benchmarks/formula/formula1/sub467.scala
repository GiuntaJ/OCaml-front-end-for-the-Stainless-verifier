import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub467 {
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
  
  def expr2int(n: Expr): Int63 = {
    n match {
      case NUM(x) => { x }
      case PLUS(x, y) => { expr2int(x) + expr2int(y) }
      case MINUS(x, y) => { expr2int(x) - expr2int(y) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f_0) => {
        eval(f_0) match {
          case true => { false }
          case false => { true }
        }
      }
      case ANDALSO(a, b) => {
        
          if (
            eval(a) eq false
          ) {
            false 
          } else if (
            eval(b) eq false
          ) {
            false 
          } else {
            true
          }
      }
      case ORELSE(a, b) => {
        
          if (
            eval(a) eq true
          ) {
            true 
          } else if (
            eval(b) eq true
          ) {
            true 
          } else {
            false
          }
      }
      case IMPLY(a, b) => {
        
          if (
            eval(a) eq false
          ) {
            true 
          } else if (
            eval(b) eq true
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(x, y) => { if (expr2int(x) < expr2int(y)) true else false }
    }
  }
}