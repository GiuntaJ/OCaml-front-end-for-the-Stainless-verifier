import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub374 {
  /*Computer Science Engineering 2015-12683 Kim Jaein*/
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
  
  def toint(value: Expr): Int63 = {
    value match {
      case NUM(i) => { i }
      case PLUS(x, y) => { toint(x) + toint(y) }
      case MINUS(x, y) => { toint(x) - toint(y) }
    }
  }
  
  def eval(value: Formula): Boolean = {
    value match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(negation) => { not(eval(negation)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case IMPLY(a, b) => { not(eval(a)) || eval(b) }
      case LESS(a, b) => { toint(a) < toint(b) }
    }
  }
}
