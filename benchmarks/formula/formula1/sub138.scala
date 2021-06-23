import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub138 {
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
  
  def doexpr(x: Expr): Int63 = {
    x match {
      case NUM(a) => { a }
      case PLUS(a, b) => { doexpr(a) + doexpr(b) }
      case MINUS(a, b) => { doexpr(a) - doexpr(b) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { not(eval(a)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case IMPLY(a, b) => { if (eval(a)) eval(b) else true }
      case LESS(a, b) => { if (doexpr(a) < doexpr(b)) true else false }
    }
  } 
}