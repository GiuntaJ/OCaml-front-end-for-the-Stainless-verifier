import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub452 {
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
  
  def comp(exp): Int63 = {
    exp match {
      case NUM(int1) => { int1 }
      case PLUS(ex1, ex2) => { comp(ex1) + comp(ex2) }
      case MINUS(ex1, ex2) => { comp(ex1) - comp(ex2) }
    }
  }
  
  
  def imply(((b1, b2))) = { if (b1 eq true && b2 eq false) false else true }
  
  def eval(formul: Formula): Boolean = {
    formul match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form) => { not(eval(form)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(f1, f2) => { imply(eval(f1), eval(f2)) }
      case LESS(e1, e2) => { comp(e1) < comp(e2) }
    }
  }
  
  /*test*//*
  let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
        test_case(1, true = eval TRUE); 
          test_case(2, false = eval FALSE); 
  	  test_case(3, false = eval (NOT TRUE)); 
  	    test_case(4, true = eval (NOT FALSE)); 
  	      test_case(5, true = eval (ANDALSO (TRUE, TRUE))); 
  	        test_case(6, false = eval (ANDALSO (TRUE, FALSE))); 
  		  test_case(7, false = eval (ANDALSO (FALSE, TRUE))); 
  		    test_case(8, false = eval (ANDALSO (FALSE, FALSE))); 
  		      test_case(9, true = eval (ORELSE (TRUE, TRUE))); 
  		        test_case(10, true = eval (ORELSE (TRUE, FALSE))); 
  			  test_case(11, true = eval (ORELSE (FALSE, TRUE))); 
  			    test_case(12, false = eval (ORELSE (FALSE, FALSE))); 
  			      test_case(13, false = eval (IMPLY (TRUE, FALSE))); 
  			        test_case(14, true = eval (IMPLY (TRUE, TRUE))); 
  				  test_case(15, true = eval (IMPLY (FALSE, TRUE))); 
  				    test_case(16, true = eval (IMPLY (FALSE, FALSE))); 
  				      test_case(17, true = eval (LESS (NUM 3, NUM 5))); 
  				        test_case(18, false = eval (LESS (NUM 3, NUM 3))); 
  					  test_case(19, false = eval (LESS (NUM 3, NUM 1))); 
  					    test_case(20, false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
  					      test_case(21, true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
  					      */
}