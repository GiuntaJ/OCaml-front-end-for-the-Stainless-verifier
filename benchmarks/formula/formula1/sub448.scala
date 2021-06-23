import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub448 {
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
  
  def evalexpr(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(e1, e2) => { evalexpr(e1) + evalexpr(e2) }
      case MINUS(e1, e2) => { evalexpr(e1) - evalexpr(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(ff) => {
        eval(ff) match {
          case true => { false }
          case false => { true }
        }
      }
      case ANDALSO(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (true, true) => { true }
          case _ => { false }
        }
      }
      case ORELSE(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (false, false) => { false }
          case _ => { true }
        }
      }
      case IMPLY(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (false, _) => { true }
          case (true, true) => { true }
          case _ => { false }
        }
      }
      case LESS(e1, e2) => { if (evalexpr(e1) < evalexpr(e2)) true else false }
    }
  }
}