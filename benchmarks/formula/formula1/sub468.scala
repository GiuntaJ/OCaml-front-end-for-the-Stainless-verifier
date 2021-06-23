import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub468 {
  /* Mechanical & Aerospace Eng./2013-11706/Kang Injae/2-1.ml */
  
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
  
  def e2i(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(m, n) => { e2i(m) + e2i(n) }
      case MINUS(m, n) => { e2i(m) - e2i(n) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { not(eval(p)) }
      case ANDALSO(p, q) => { eval(p) && eval(q) }
      case ORELSE(p, q) => { eval(p) || eval(q) }
      case IMPLY(p, q) => {
        if (eval(p) == true && eval(q) == false) false else true
      }
      case LESS(m, n) => { e2i(m) < e2i(n) }
    }
  }
}