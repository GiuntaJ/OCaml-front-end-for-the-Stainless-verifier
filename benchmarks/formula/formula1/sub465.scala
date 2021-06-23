import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub465 {
  /* 컴퓨터공학부 2013-11425 이창영 hw2_1 */
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def valofexpr(e: Expr): Int63 = {
    e match {
      case NUM(a) => { a }
      case PLUS(a, b) => { valofexpr(a) + valofexpr(b) }
      case MINUS(a, b) => { valofexpr(a) - valofexpr(b) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { not(eval(a)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
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
      case LESS(a, b) => { if (valofexpr(a) < valofexpr(b)) true else false }
    }
  }
}