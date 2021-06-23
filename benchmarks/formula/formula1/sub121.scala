import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub121 {
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
  
  def num(exp: Expr): Int63 = {
    exp match {
      case NUM(x) => { x }
      case PLUS(x, y) => { num(x) + num(y) }
      case MINUS(x, y) => { num(x) - num(y) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => {
        if (eval(x) == true && eval(y) == true) true else false
      }
      case ORELSE(x, y) => {
        if (eval(x) == false && eval(y) == false) false else true
      }
      case IMPLY(x, y) => {
        if (eval(x) == true && eval(y) == false) false else true
      }
      case LESS(x, y) => { if (num(x) < num(y)) true else false }
    }
  }
  	
}