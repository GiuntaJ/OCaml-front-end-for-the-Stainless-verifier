import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub68 {
  /* 2004-11951 Noh, Soon Hyun */
  
  /* skeleton type from TA */
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
  
  /* to evaluete expressions */
  def evalexpr(v: Expr): Int63 = {
    v match {
      case NUM(x) => { x }
      case PLUS(x, y) => { evalexpr(x) + evalexpr(y) }
      case MINUS(x, y) => { evalexpr(x) - evalexpr(y) }
    }
  }
  
  /* middle-level function of eval */
  /* If I use <formula> -> <bool> function direct to recursion,
  I cannot use return value to its argument again */
  /* type of protoeval f is <formula> -> <formula> */
  def protoeval(f: Formula): Formula = {
    f match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(TRUE) => { FALSE }
      case NOT(FALSE) => { TRUE }
      case NOT(x) => { protoeval(NOT(protoeval(x))) }
      case ANDALSO(FALSE, FALSE) => { FALSE }
      case ANDALSO(TRUE, FALSE) => { FALSE }
      case ANDALSO(FALSE, TRUE) => { FALSE }
      case ANDALSO(TRUE, TRUE) => { TRUE }
      case ANDALSO(x, y) => { protoeval(ANDALSO(protoeval(x), protoeval(y))) }
      case ORELSE(FALSE, FALSE) => { FALSE }
      case ORELSE(TRUE, FALSE) => { TRUE }
      case ORELSE(FALSE, TRUE) => { TRUE }
      case ORELSE(TRUE, TRUE) => { TRUE }
      case ORELSE(x, y) => { protoeval(ORELSE(protoeval(x), protoeval(y))) }
      case IMPLY(FALSE, FALSE) => { TRUE }
      case IMPLY(TRUE, FALSE) => { FALSE }
      case IMPLY(FALSE, TRUE) => { TRUE }
      case IMPLY(TRUE, TRUE) => { TRUE }
      case IMPLY(x, y) => { protoeval(IMPLY(protoeval(x), protoeval(y))) }
      case LESS(a, b) => { if (evalexpr(a) < evalexpr(b)) TRUE else FALSE }
    }
  }
  
  /* main function */
  def eval(f: Formula): Boolean = {
    
      if (
        protoeval(f) == TRUE
      ) {
        true 
      } else if (
        protoeval(f) == FALSE
      ) {
        false 
      } else {
        false
      }
  }
  
  /* Test Code :: some parts are refrenced from last semester class web board
  let test = eval(LESS(PLUS(NUM 5, NUM 5), MINUS(NUM 20, NUM 13)))
  let print_bool a =
  	if a=true then print_string "true
  "
  	else print_string "false
  "
  
  let f p q = eval (ANDALSO (p, (ORELSE (q, (ANDALSO ((NOT p), (NOT q)))))));; 
  let r_f p q = not (f p q);;
  let _ = print_bool (f TRUE TRUE) 
  let _ = print_bool (f TRUE FALSE) 
  let _ = print_bool (f FALSE TRUE) 
  let _ = print_bool (f FALSE FALSE) 
  let _ = print_bool (r_f TRUE TRUE) 
  let _ = print_bool (r_f TRUE FALSE) 
  let _ = print_bool (r_f FALSE TRUE) 
  let _ = print_bool (r_f FALSE FALSE)
  
  let _ = print_bool test
  */
}