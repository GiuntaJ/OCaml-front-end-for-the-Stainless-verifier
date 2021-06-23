import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub449 {
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
  
  def eval_number(f: Expr): Int63 = {
    f match {
      case NUM(f) => { f }
      case PLUS(f1, f2) => { eval_number(f1) + eval_number(f2) }
      case MINUS(f1, f2) => { eval_number(f1) - eval_number(f2) }
    }
  }
  	
  def eval_formula(f: Formula): Formula = {
    f match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(TRUE) => { FALSE }
      case NOT(FALSE) => { TRUE }
      case NOT(f1) => { eval_formula(NOT(eval_formula(f1))) }
      case ANDALSO(TRUE, TRUE) => { TRUE }
      case ANDALSO(FALSE, _) => { FALSE }
      case ANDALSO(_, FALSE) => { FALSE }
      case ANDALSO(f1, f2) => {
        eval_formula(ANDALSO(eval_formula(f1), eval_formula(f2)))
      }
      case ORELSE(TRUE, _) => { TRUE }
      case ORELSE(_, TRUE) => { TRUE }
      case ORELSE(FALSE, FALSE) => { FALSE }
      case ORELSE(f1, f2) => {
        eval_formula(ORELSE(eval_formula(f1), eval_formula(f2)))
      }
      case IMPLY(TRUE, TRUE) => { TRUE }
      case IMPLY(TRUE, FALSE) => { FALSE }
      case IMPLY(FALSE, _) => { TRUE }
      case IMPLY(f1, f2) => {
        eval_formula(IMPLY(eval_formula(f1), eval_formula(f2)))
      }
      case LESS(f1, f2) => {
        if (eval_number(f1) < eval_number(f2)) TRUE else FALSE
      }
    }
  }
  def eval(f: Formula): Boolean = { if (eval_formula(f) eq TRUE) true else false
  }
}
