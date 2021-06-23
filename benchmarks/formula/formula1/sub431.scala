import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub431 {
  /*
    CSE 2012-11226 Kwak Jin Han
  	exercise 1
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
  
  /* exprtoint : expr -> int */
  def exprtoint(e: Expr): Int63 = {
    e match {
      case NUM(num) => { num }
      case PLUS(x, y) => { exprtoint(x) + exprtoint(y) }
      case MINUS(x, y) => { exprtoint(x) - exprtoint(y) }
    }
  }
  
  /* eval : formula -> bool */
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(pf) => { not(eval(pf)) }
      case ANDALSO(lf, rf) => { eval(lf) && eval(rf) }
      case ORELSE(lf, rf) => { eval(lf) || eval(rf) }
      case IMPLY(lf, rf) => { if (eval(lf) == false) true else eval(rf) }
      case LESS(le, re) => { if (exprtoint(le) < exprtoint(re)) true else false
      }
    }
  }
  
  /*
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool (true = eval (NOT (NOT (NOT FALSE))));
  print_bool (true = eval TRUE); 
  print_bool (false = eval FALSE); 
  print_bool (false = eval (NOT TRUE)); 
  print_bool (true = eval (NOT FALSE)); 
  print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
  print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
  print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
  print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
  print_bool (true = eval (ORELSE (TRUE, TRUE))); 
  print_bool (true = eval (ORELSE (TRUE, FALSE))); 
  print_bool (true = eval (ORELSE (FALSE, TRUE))); 
  print_bool (false = eval (ORELSE (FALSE, FALSE))); 
  print_bool (false = eval (IMPLY (TRUE, FALSE))); 
  print_bool (true = eval (IMPLY (TRUE, TRUE))); 
  print_bool (true = eval (IMPLY (FALSE, TRUE))); 
  print_bool (true = eval (IMPLY (FALSE, FALSE))); 
  print_bool (true = eval (LESS (NUM 3, NUM 5))); 
  print_bool (false = eval (LESS (NUM 3, NUM 3))); 
  print_bool (false = eval (LESS (NUM 3, NUM 1))); 
  print_bool (false = eval 
  		(LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
  print_bool (true = eval 
  		(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 
  print_bool (true = eval (ANDALSO (NOT (FALSE), NOT (NOT (NOT FALSE)))));	
  print_bool (false = eval (ORELSE (NOT (NOT (NOT TRUE)), NOT (TRUE))));
  */
}