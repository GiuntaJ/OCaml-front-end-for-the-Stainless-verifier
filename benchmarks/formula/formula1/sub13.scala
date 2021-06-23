import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub13 {
  /* Department: EE
   * Student No.: 2009-20769
   * Name: Kim, Seongjun
   * Exercise 3
   */
  
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
  
  
  def eval(formula: Formula): Boolean = {
    val _2 = {
      def eval_expr(expr: Expr) = {
        expr match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { eval_expr(e1) + eval_expr(e2) }
          case MINUS(e1, e2) => { eval_expr(e1) - eval_expr(e2) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { not(eval(f)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
        case LESS(e1, e2) => { eval_expr(e1) < eval_expr(e2) }
      }
    }
  }
  /*
  ;;
  assert (eval TRUE = true);;
  assert (eval FALSE = false);;
  
  assert (eval (NOT TRUE) = false);;
  assert (eval (NOT FALSE) = true);;
  
  assert (eval (ANDALSO (TRUE, TRUE)) = true);;
  assert (eval (ANDALSO (FALSE, TRUE)) = false);;
  assert (eval (ANDALSO (TRUE, FALSE)) = false);;
  assert (eval (ANDALSO (FALSE, FALSE)) = false);;
  
  assert (eval (ORELSE (TRUE, TRUE)) = true);;
  assert (eval (ORELSE (FALSE, TRUE)) = true);;
  assert (eval (ORELSE (TRUE, FALSE)) = true);;
  assert (eval (ORELSE (FALSE, FALSE)) = false);;
  
  assert (eval (IMPLY (TRUE, TRUE)) = true);;
  assert (eval (IMPLY (FALSE, TRUE)) = true);;
  assert (eval (IMPLY (TRUE, FALSE)) = false);;
  assert (eval (IMPLY (FALSE, FALSE)) = true);;
  
  assert (eval_expr (NUM 3) = 3);;
  assert (eval_expr (PLUS (NUM 3, NUM 3)) = 6);;
  assert (eval_expr (MINUS (NUM 4, NUM 3)) = 1);;
  
  assert (eval (LESS (NUM 2, (PLUS (MINUS (NUM 3, NUM 2), NUM 7)))) = true);;
  */
}