import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub485 {
  /*컴퓨터공학부 2014-16775 김민지
  priogramming language hw 2-1*/
  
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
  
  def calcul(x: Expr): Int63 = {
    x match {
      case NUM(n) => { n }
      case PLUS(e1, e2) => { calcul(e1) + calcul(e2) }
      case MINUS(e1, e2) => { calcul(e1) - calcul(e2) }
    }
  }
  
  def eval(x: Formula): Boolean = {
    x match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x1) => { not(eval(x1)) }
      case ANDALSO(x1, x2) => {
        (eval(x1), eval(x2)) match {
          case (true, true) => { true }
          case (true, false) => { false }
          case (false, _) => { false }
        }
      }
      case ORELSE(x1, x2) => {
        (eval(x1), eval(x2)) match {
          case (false, false) => { false }
          case (false, true) => { true }
          case (true, _) => { true }
        }
      }
      case IMPLY(x1, x2) => {
        (eval(x1), eval(x2)) match {
          case (true, true) => { true }
          case (true, false) => { false }
          case (false, _) => { true }
        }
      }
      case LESS(x1, x2) => { if (calcul(x1) < calcul(x2)) true else false }
    }
  }
}
