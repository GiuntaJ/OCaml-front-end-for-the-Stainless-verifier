import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub486 {
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
  
  def eval_expr: Expr => Int63 = (
    (expr_to_evaluate) =>
      {
        expr_to_evaluate match {
          case NUM(n) => { n }
          case PLUS(expr1_, expr2_) => { eval_expr(expr1_) + eval_expr(expr2_) }
          case MINUS(expr1_, expr2_) => { eval_expr(expr1_) - eval_expr(expr2_)
          }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        form match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(form_) => { if (eval(form_)) false else true }
          case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
          case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
          case IMPLY(form1, form2) => { if (eval(form1)) eval(form2) else true }
          case LESS(expr1, expr2) => { eval_expr(expr1) < eval_expr(expr2) }
        }
    }
  )
}
