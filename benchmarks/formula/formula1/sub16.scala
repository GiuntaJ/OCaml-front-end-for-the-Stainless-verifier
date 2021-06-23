import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub16 {
  /* Computer Science/2005-11759/Sangcheol Park/Exercise 2-3.*/
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
    val _2 = {
      def eval_expr(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(a, b) => { eval_expr(a) + eval_expr(b) }
          case MINUS(a, b) => { eval_expr(a) - eval_expr(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { eval(a) }
        case ANDALSO(a, b) => { eval(a) && eval(b) }
        case ORELSE(a, b) => { eval(a) || eval(b) }
        case IMPLY(a, b) => { not(eval(a) && not(eval(b))) }
        case LESS(a, b) => { eval_expr(a) < eval_expr(b) }
      }
    }
  }
  
  /* 
  eval TRUE;;
  eval FALSE;;
  eval(NOT TRUE);;
  eval(NOT FALSE);;
  eval(ANDALSO(TRUE, FALSE));;
  eval(ORELSE(TRUE, FALSE));;
  eval(IMPLY(TRUE, FALSE));;
  eval(IMPLY(FALSE, FALSE));;
  eval(IMPLY(FALSE, TRUE));;
  eval(IMPLY(TRUE, TRUE));;
  eval(ANDALSO(LESS(PLUS(NUM 10, NUM 20), MINUS(NUM 100, PLUS(NUM 30, NUM 40))), TRUE));;
  */
}