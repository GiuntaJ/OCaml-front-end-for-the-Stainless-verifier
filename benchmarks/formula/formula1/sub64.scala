import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub64 {
  /* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-5 */
  
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
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(x) => { x }
      case PLUS(a, b) => { calc(a) + calc(b) }
      case MINUS(a, b) => { calc(a) - calc(b) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { if (eval(x) eq true) false else true }
      case ANDALSO(a, b) => {
        if (eval(a) eq true && eval(b) eq true) true else false
      }
      case ORELSE(a, b) => {
        if (eval(a) eq false && eval(b) eq false) false else true
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
      case LESS(a, b) => { if (calc(a) < calc(b)) true else false }
    }
  }
  
  
  /* test code */
}
