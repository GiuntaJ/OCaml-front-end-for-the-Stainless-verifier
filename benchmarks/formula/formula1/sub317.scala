import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub317 {
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
  
  def calculator(n: Expr): Int63 = {
    n match {
      case NUM(i) => { i }
      case PLUS(a, b) => { calculator(a) + calculator(b) }
      case MINUS(a, b) => { calculator(a) - calculator(b) }
    }
  }
  
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form_0) => {
        eval(form_0) match {
          case true => { false }
          case false => { true }
        }
      }
      case ANDALSO(f1, f2) => {
        eval(f1) match {
          case false => { false }
          case true => { eval(f2) }
        }
      }
      case ORELSE(f1, f2) => {
        eval(f1) match {
          case true => { true }
          case false => { eval(f2) }
        }
      }
      case IMPLY(p, q) => {
        eval(p) match {
          case false => { true }
          case true => { eval(q) }
        }
      }
      case LESS(a1, a2) => {
        if (calculator(a1) - calculator(a2) < 0) true else false
      }
    }
  }
}