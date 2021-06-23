import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub41 {
  sealed case class Error() extends Exception {}
  
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
  
  def value(exp: Expr): Int63 = {
    exp match {
      case NUM(num) => { num }
      case PLUS(num1, num2) => { value(num1) + value(num2) }
      case MINUS(num1, num2) => { value(num1) - value(num2) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form1) => { not(eval(form1)) }
      case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
      case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
      case IMPLY(form1, form2) => { not(eval(form1)) || eval(form2) }
      case LESS(exp1, exp2) => { value(exp1) < value(exp2) }
    }
  }
}