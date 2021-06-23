import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub413 {
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
  
  def evalexp(exp: Expr): Int63 = {
    exp match {
      case NUM(a) => { a }
      case PLUS(a, b) => { evalexp(a) + evalexp(b) }
      case MINUS(a, b) => { evalexp(a) - evalexp(b) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => {
        eval(f) match {
          case true => { false }
          case false => { true }
        }
      }
      case ANDALSO(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (true, true) => { true }
          case (true, false) | (false, true) | (false, false) => { false }
        }
      }
      case ORELSE(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (true, true) | (true, false) | (false, true) => { true }
          case (false, false) => { false }
        }
      }
      case IMPLY(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (true, false) => { false }
          case (true, true) | (false, true) | (false, false) => { true }
        }
      }
      case LESS(e1, e2) => { evalexp(e1) < evalexp(e2) }
    }
  }
  
    
}