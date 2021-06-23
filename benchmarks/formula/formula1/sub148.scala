import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub148 {
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
  
  def ex(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(p1, p2) => { ex(p1) + ex(p2) }
      case MINUS(m1, m2) => { ex(m1) - ex(m2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(n) => { if (eval(n) == true) false else true }
      case ANDALSO(a1, a2) => {
        if (eval(a1) == true && eval(a2) == true) true else false
      }
      case ORELSE(o1, o2) => {
        if (eval(o1) == false && eval(o2) == false) false else true
      }
      case IMPLY(i1, i2) => {
        if (eval(i1) == true && eval(i2) == false) false else true
      }
      case LESS(l1, l2) => { if (ex(l1) < ex(l2)) true else false }
    }
  }
}