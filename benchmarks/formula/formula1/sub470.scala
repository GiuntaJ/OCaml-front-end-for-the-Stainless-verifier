import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub470 {
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
  
  def expr_to_int(x: Expr): Int63 = {
    x match {
      case NUM(x) => { x }
      case PLUS(a, b) => { expr_to_int(a) + expr_to_int(b) }
      case MINUS(a, b) => { expr_to_int(a) - expr_to_int(b) }
    }
  }
  
  def eval_less(((x: Int63, y: Int63))): Boolean = { x < y }
  
  def bool_to_formula(b: Boolean): Formula = {
    b match {
      case true => { TRUE }
      case false => { FALSE }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => {
        a match {
          case TRUE => { false }
          case FALSE => { true }
          case a => { eval(NOT(bool_to_formula(eval(a)))) }
        }
      }
      case ANDALSO(a, b) => {
        (a, b) match {
          case (TRUE, TRUE) => { true }
          case (TRUE, FALSE) => { false }
          case (FALSE, TRUE) => { false }
          case (FALSE, FALSE) => { false }
          case (a, b) => {
            eval(ANDALSO(bool_to_formula(eval(a)), bool_to_formula(eval(b))))
          }
        }
      }
      case ORELSE(a, b) => {
        (a, b) match {
          case (TRUE, TRUE) => { true }
          case (TRUE, FALSE) => { true }
          case (FALSE, TRUE) => { true }
          case (FALSE, FALSE) => { false }
          case (a, b) => {
            eval(ORELSE(bool_to_formula(eval(a)), bool_to_formula(eval(b))))
          }
        }
      }
      case IMPLY(a, b) => {
        (a, b) match {
          case (TRUE, TRUE) => { true }
          case (FALSE, TRUE) => { true }
          case (TRUE, FALSE) => { false }
          case (FALSE, FALSE) => { true }
          case (a, b) => {
            eval(IMPLY(bool_to_formula(eval(a)), bool_to_formula(eval(b))))
          }
        }
      }
      case LESS(c, d) => { eval_less(expr_to_int(c), expr_to_int(d)) }
    }
  }
}