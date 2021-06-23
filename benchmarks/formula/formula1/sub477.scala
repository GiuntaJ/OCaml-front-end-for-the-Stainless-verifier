import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub477 {
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
      case NUM(i) => { i }
      case PLUS(a, b) => { calc(a) + calc(b) }
      case MINUS(a, b) => { calc(a) - calc(b) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { eval(a) }
      case ANDALSO(a, b) => { if (eval(a) == true) eval(b) else false }
      case ORELSE(a, b) => { if (eval(a) == false) eval(b) else true }
      case IMPLY(a, b) => { if (eval(a) == true) eval(b) else true }
      case LESS(a, b) => { if (calc(a) < calc(b)) true else false }
    }
  }
}
