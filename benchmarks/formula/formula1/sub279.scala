import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub279 {
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
   
   def exp1(a) = {
    a match {
      case NUM(num) => { num }
      case PLUS(num1, num2) => { exp1(num1) + exp1(num2) }
      case MINUS(num1, num2) => { exp1(num1) - exp1(num2) }
    }
  }
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => { eval(x) && eval(y) }
      case ORELSE(x, y) => { eval(x) || eval(y) }
      case IMPLY(x, y) => { eval(x) == false || eval(y) == true }
      case LESS(x, y) => { exp1(x) < exp1(y) }
    }
  }
}