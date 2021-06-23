import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub204 {
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
  
  def expr2num(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(n1, n2) => { expr2num(n1) + expr2num(n2) }
      case MINUS(n1, n2) => { expr2num(n1) - expr2num(n2) }
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
      case LESS(e1, e2) => { expr2num(e1) < expr2num(e2) }
    }
  }
}