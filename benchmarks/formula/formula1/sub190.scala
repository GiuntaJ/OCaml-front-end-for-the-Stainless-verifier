import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub190 {
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
  
  def cal(e: Expr): Int63 = {
    e match {
      case NUM(num) => { num }
      case PLUS(lexpr, rexpr) => { cal(lexpr) + cal(rexpr) }
      case MINUS(lexpr, rexpr) => { cal(lexpr) - cal(rexpr) }
    }
  }	
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form) => { not(eval(form)) }
      case ANDALSO(lform, rform) => { eval(lform) && eval(rform) }
      case ORELSE(lform, rform) => { eval(lform) || eval(rform) }
      case IMPLY(lform, rform) => { not(eval(lform)) || eval(rform) }
      case LESS(lexpr, rexpr) => { cal(lexpr) < cal(rexpr) }
    }
  }
}