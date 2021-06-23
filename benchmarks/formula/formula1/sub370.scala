import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub370 {
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
  
  def value(a: Expr): Int63 = {
    a match {
      case NUM(x) => { x }
      case PLUS(x, y) => { value(x) + value(y) }
      case MINUS(x, y) => { value(x) - value(y) }
    }
  }
  
  def eval(a: Formula): Boolean = {
    a match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => {
        
          if (
            eval(x) == false
          ) {
            false 
          } else if (
            eval(y) == false
          ) {
            false 
          } else {
            true
          }
      }
      case ORELSE(x, y) => {
        
          if (
            eval(x) == true
          ) {
            true 
          } else if (
            eval(y) == true
          ) {
            true 
          } else {
            false
          }
      }
      case IMPLY(x, y) => {
        
          if (
            eval(x) == false
          ) {
            true 
          } else if (
            eval(y) == true
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(x, y) => { if (value(x) < value(y)) true else false }
    }
  }
}
