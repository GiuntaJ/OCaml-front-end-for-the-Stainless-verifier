import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub391 {
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
  
  def eval_help(e: Expr): Int63 = {
    e match {
      case NUM(x) => { x }
      case PLUS(x, y) => { eval_help(x) + eval_help(y) }
      case MINUS(x, y) => { eval_help(x) - eval_help(y) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => { eval(x) && eval(y) }
      case ORELSE(x, y) => { eval(x) || eval(y) }
      case IMPLY(x, y) => { not(eval(x)) || eval(y) }
      case LESS(x, y) => { eval_help(x) < eval_help(y) }
    }
  }
  
}
