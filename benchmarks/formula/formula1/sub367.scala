import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub367 {
  /* 2015-11380 박찬양 HW2_1 */
  
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
  
  def getnum: Expr => Int63 = (
    (exp) =>
      {
        exp match {
          case NUM(a) => { a }
          case PLUS(a, b) => { getnum(a) + getnum(b) }
          case MINUS(a, b) => { getnum(a) - getnum(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        form match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => { if (eval(a) == true) false else true }
          case ANDALSO(a, b) => { eval(a) && eval(b) }
          case ORELSE(a, b) => { eval(a) || eval(b) }
          case IMPLY(a, b) => { not(eval(a)) || eval(b) }
          case LESS(a, b) => { if (getnum(a) < getnum(b)) true else false }
        }
    }
  ) 
}
