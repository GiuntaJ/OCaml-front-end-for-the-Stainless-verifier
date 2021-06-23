import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub297 {
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
  
  def ecal(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(i, j) => { ecal(i) + ecal(j) }
      case MINUS(i, j) => { ecal(i) - ecal(j) }
    }
  }  
  
  def eval(p: Formula): Boolean = {
    p match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { if (eval(p) eq true) false else true }
      case ANDALSO(p, q) => {
        if (eval(p) eq true && eval(q) eq true) true else false
      }
      case ORELSE(p, q) => {
        if (eval(p) eq false && eval(q) eq false) false else true
      }
      case IMPLY(p, q) => {
        if (eval(p) eq true && eval(q) eq false) false else true
      }
      case LESS(e1, e2) => { if (ecal(e1) < ecal(e2)) true else false }
    }
  }
  
}
