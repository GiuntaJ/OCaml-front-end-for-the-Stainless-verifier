import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub372 {
  
  /*real code start*/
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
      case PLUS(x, y) => { calc(x) + calc(y) }
      case MINUS(x, y) => { calc(x) - calc(y) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f1) => { if (eval(f1) eq true) false else true }
      case ANDALSO(f1, f2) => {
        if (eval(f1) eq true && eval(f2) eq true) true else false
      }
      case ORELSE(f1, f2) => {
        if (eval(f1) eq false && eval(f2) eq false) false else true
      }
      case IMPLY(f1, f2) => {
        if (eval(f1) eq false || eval(f2) eq true) true else false
      }
      case LESS(e1, e2) => { if (calc(e1) < calc(e2)) true else false }
    }
  }
  /*real code end*/
}