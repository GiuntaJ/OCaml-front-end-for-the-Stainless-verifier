import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub272 {
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
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form) => { not(eval(form)) }
      case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
      case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
      case IMPLY(form1, form2) => { not(eval(form1)) || eval(form2) }
      case LESS(expr1, expr2) => { ex(expr1) < ex(expr2) }
    }
  }
  def ex(expre) = {
    expre match {
      case NUM(exp0) => { exp0 }
      case PLUS(exp1, exp2) => { ex(exp1) + ex(exp2) }
      case MINUS(exp1, exp2) => { ex(exp1) - ex(exp2) }
    }
  }
}