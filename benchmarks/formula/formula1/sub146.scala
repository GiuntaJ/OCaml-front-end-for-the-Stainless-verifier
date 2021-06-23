import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub146 {
  
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
  
  def eval(form: Formula): Boolean = {
    val _2 = {
      def int_of_expr(expr_in) = {
        expr_in match {
          case NUM(x) => { x }
          case PLUS(expr1, expr2) => { int_of_expr(expr1) + int_of_expr(expr2) }
          case MINUS(expr1, expr2) => { int_of_expr(expr1) - int_of_expr(expr2)
          }
        }
      }
      form match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(form1) => { not(eval(form1)) }
        case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
        case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
        case IMPLY(form1, form2) => { not(eval(form1)) || eval(form2) }
        case LESS(expr1, expr2) => { int_of_expr(expr1) < int_of_expr(expr2) }
      }
    }
  }
  
  /* exercise test
  Printf.printf "eval : %b
  " (eval (NOT(LESS(NUM 10, NUM 2))));;
  exercise */
}