import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub31 {
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
  
  def calc(var_expr: Expr): Int63 = {
    var_expr match {
      case NUM(int_var) => { int_var }
      case PLUS(expr1, expr2) => { calc(expr1) + calc(expr2) }
      case MINUS(expr1, expr2) => { calc(expr1) - calc(expr2) }
    }
  }
  
  def eval(var_form: Formula): Boolean = {
    var_form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form1) => { if (eval(form1) eq true) false else true }
      case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
      case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
      case IMPLY(form1, form2) => {
        if (eval(form1) eq false) true else eval(form2)
      }
      case LESS(expr1, expr2) => {
        if (calc(expr1) < calc(expr2)) true else false
      }
    }
  }
}