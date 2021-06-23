import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub22 {
  /* 2006-11782 Song Young-chan, Hw2-3 True/False */
  
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
  
  def eval_expr(expression: Expr): Int63 = {
    expression match {
      case NUM(number) => { number }
      case PLUS(num1, num2) => { eval_expr(num1) + eval_expr(num2) }
      case MINUS(num1, num2) => { eval_expr(num1) - eval_expr(num2) }
    }
  }
  
  def eval(input_formula: Formula): Boolean = {
    input_formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(t_f) => { if (eval(t_f) == true) false else true }
      case ANDALSO(t_f1, t_f2) => { eval(t_f1) && eval(t_f2) }
      case ORELSE(t_f1, t_f2) => { eval(t_f1) || eval(t_f2) }
      case IMPLY(t_f1, t_f2) => {
        if (eval(t_f1) == false || eval(t_f2) == true) true else false
      }
      case LESS(expr1, expr2) => {
        if (eval_expr(expr1) < eval_expr(expr2)) true else false
      }
    }
  }
}