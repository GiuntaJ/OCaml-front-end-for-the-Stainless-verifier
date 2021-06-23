import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub127 {
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
  
  
  def calc(exp: Expr): Int63 = {
    exp match {
      case NUM(exp) => { exp }
      case PLUS(expa, expb) => { calc(expa) + calc(expb) }
      case MINUS(expa, expb) => { calc(expa) - calc(expb) }
    }
  }
  
  def eval(exp: Formula): Boolean = {
    exp match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(expa) => { not(eval(expa)) }
      case ANDALSO(expa, expb) => { eval(expa) && eval(expb) }
      case ORELSE(expa, expb) => { eval(expa) || eval(expb) }
      case IMPLY(expa, expb) => { not(eval(expa)) || eval(expb) }
      case LESS(expa, expb) => { if (calc(expa) < calc(expb)) true else false }
    }
  }
}