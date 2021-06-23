import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub296 {
  /* Ex 4. True or False */
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
  
  def calc_expr(e: Expr): Int63 = {
    e match {
      case NUM(e) => { e }
      case PLUS(e1, e2) => { calc_expr(e1) + calc_expr(e2) }
      case MINUS(e1, e2) => { calc_expr(e1) - calc_expr(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(sf) => { not(eval(sf)) }
      case ANDALSO(sf1, sf2) => { eval(sf1) && eval(sf2) }
      case ORELSE(sf1, sf2) => { eval(sf1) || eval(sf2) }
      case IMPLY(sf1, sf2) => {
        eval(sf1) match {
          case true => { eval(sf2) }
          case false => { true }
        }
      }
      case LESS(e1, e2) => { if (calc_expr(e1) < calc_expr(e2)) true else false
      }
    }
  }
  /*
  let _ =
  	let msg = string_of_bool (eval TRUE) in
  	print_endline msg
  
  let _ =
  	let msg = string_of_bool (eval (ANDALSO(TRUE, FALSE))) in
  	print_endline msg
  
  let _ =
  	let msg = string_of_bool (eval (LESS(NUM 3, PLUS(NUM 2, NUM 4)))) in
  	print_endline msg
  */
}