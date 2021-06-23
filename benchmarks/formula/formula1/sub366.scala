import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub366 {
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
  
  def value(ex: Expr): Int63 = {
    ex match {
      case NUM(x) => { x }
      case PLUS(x, y) => { value(x) + value(y) }
      case MINUS(x, y) => { value(x) - value(y) }
    }
  }
  def eval(fm: Formula): Boolean = {
    fm match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { eval(x) eq false }
      case ANDALSO(x, y) => { eval(x) && eval(y) }
      case ORELSE(x, y) => { eval(x) || eval(y) }
      case IMPLY(x, y) => { eval(x) eq false || eval(y) }
      case LESS(x, y) => { value(x) < value(y) }
    }
  }
}