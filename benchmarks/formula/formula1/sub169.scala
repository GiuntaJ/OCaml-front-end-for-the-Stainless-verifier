import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub169 {
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
  
  def etoi(exp: Expr): Int63 = {
    exp match {
      case NUM(a) => { a }
      case PLUS(exp1, exp2) => { etoi(exp1) + etoi(exp2) }
      case MINUS(exp1, exp2) => { etoi(exp1) - etoi(exp2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { not(eval(p)) }
      case ANDALSO(p, q) => { eval(p) && eval(q) }
      case ORELSE(p, q) => { eval(p) || eval(q) }
      case IMPLY(p, q) => { not(eval(p)) || eval(q) }
      case LESS(exp1, exp2) => { if (etoi(exp1) < etoi(exp2)) true else false }
    }
  }
}