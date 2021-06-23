import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub286 {
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
  
  def exVal(x: Expr): Int63 = {
    x match {
      case NUM(n) => { n }
      case PLUS(a, b) => { exVal(a) + exVal(b) }
      case MINUS(c, d) => { exVal(c) - exVal(d) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(n) => { if (eval(n) == true) false else true }
      case ANDALSO(a, b) => {
        if (eval(a) == true && eval(b) == true) true else false
      }
      case ORELSE(c, d) => {
        if (eval(c) == false && eval(d) == false) false else true
      }
      case IMPLY(e, f) => {
        if (eval(e) == true && eval(f) == false) false else true
      }
      case LESS(g, h) => { if (exVal(g) < exVal(h)) true else false }
    }
  }
}