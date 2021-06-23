import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub341 {
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(le, re) => { calc(le) + calc(re) }
      case MINUS(le, re) => { calc(le) - calc(re) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form) => { not(eval(form)) }
      case ANDALSO(lf, rf) => { eval(lf) && eval(rf) }
      case ORELSE(lf, rf) => { eval(lf) || eval(rf) }
      case IMPLY(lf, rf) => { not(eval(lf)) || eval(rf) }
      case LESS(e1, e2) => { calc(e1) < calc(e2) }
    }
  }
  /*
  let form = LESS(PLUS(NUM(1),NUM(0)), MINUS(NUM(3),NUM(1)))
  let _ = print_endline (string_of_bool (eval form))
  */
}