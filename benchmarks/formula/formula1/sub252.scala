import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub252 {
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
      case NUM(n) => { n }
      case PLUS(e1, e2) => { calc(e1) + calc(e2) }
      case MINUS(e1, e2) => { calc(e1) - calc(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { if (eval(f) == true) false else true }
      case ANDALSO(f1, f2) => {
        if (eval(f1) == true && eval(f2) == true) true else false
      }
      case ORELSE(f1, f2) => {
        if (eval(f1) == false && eval(f2) == false) false else true
      }
      case IMPLY(f1, f2) => {
        if (eval(f1) == true && eval(f2) == false) false else true
      }
      case LESS(e1, e2) => { if (calc(e1) < calc(e2)) true else false }
    }
  }
}