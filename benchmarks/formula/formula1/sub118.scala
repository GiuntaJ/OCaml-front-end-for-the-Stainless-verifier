import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub118 {
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
  
  def value(exp: Expr): Int63 = {
    exp match {
      case PLUS(a, b) => { value(a) + value(b) }
      case MINUS(a, b) => { value(a) - value(b) }
      case NUM(n) => { n }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case NOT(a) => { not(eval(a)) }
      case IMPLY(a, b) => { not(eval(a)) || eval(b) }
      case LESS(a, b) => { if (value(a) < value(b)) true else false }
    }
  }
  
}
