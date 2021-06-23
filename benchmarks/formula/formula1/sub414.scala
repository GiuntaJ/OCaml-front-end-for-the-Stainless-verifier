import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub414 {
  /*컴퓨터공학부/2011-11729/안진우/2-1*/
  
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
  
  def calc(x: Expr): Int63 = {
    x match {
      case NUM(y) => { y }
      case PLUS(y, z) => { calc(y) + calc(z) }
      case MINUS(y, z) => { calc(y) - calc(z) }
    }
  }
  
  
  def eval(x: Formula): Boolean = {
    x match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(y) => { not(eval(y)) }
      case ANDALSO(y, z) => { eval(y) && eval(z) }
      case ORELSE(y, z) => { eval(y) || eval(z) }
      case IMPLY(y, z) => {
        
          if (
            eval(y) eq false
          ) {
            true 
          } else if (
            eval(z) eq true
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(y, z) => { calc(y) < calc(z) }
    }
  }
}
