import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub471 {
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
  
  def eval3(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(e1, e2) => { eval3(e1) + eval3(e2) }
      case MINUS(e1, e2) => { eval3(e1) - eval3(e2) }
    }
  }
  
  def eval2(ff: Formula): Formula = {
    ff match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(fn) => {
        eval2(fn) match {
          case TRUE => { FALSE }
          case FALSE => { TRUE }
        }
      }
      case ANDALSO(f1, f2) => {
        (eval2(f1), eval2(f2)) match {
          case (TRUE, TRUE) => { TRUE }
          case (_, _) => { FALSE }
        }
      }
      case ORELSE(f1, f2) => {
        (eval2(f1), eval2(f2)) match {
          case (FALSE, FALSE) => { FALSE }
          case (_, _) => { TRUE }
        }
      }
      case IMPLY(f1, f2) => {
        (eval2(f1), eval2(f2)) match {
          case (TRUE, FALSE) => { FALSE }
          case (_, _) => { TRUE }
        }
      }
      case LESS(e1, e2) => { if (eval3(e1) < eval3(e2)) TRUE else FALSE }
    }
  }
  
  def eval(f: Formula): Boolean = {
    eval2(f) match {
      case TRUE => { true }
      case FALSE => { false }
    }
  }
   
  /*
  let x1 : formula = ANDALSO(TRUE, FALSE) /*FALSE*/
  let x2 : formula = ORELSE(FALSE, FALSE) /*FALSE*/
  let x : formula = IMPLY(x1, x2)	 /*TRUE*/
  let x_value : bool = eval x
  
  let _ = if x_value == true then  print_endline ("O")
  		  else print_endline("X") /*O*/
  
  let y1 : expr = NUM 3
  let y2 : expr = NUM 4
  let yp : expr = PLUS(y1,y2)
  let ym : expr = MINUS(y2,y1)
  let y : formula = LESS(yp,ym) /*FALSE*/
  let y_value : bool = eval y
  
  let _ = if y_value == true then print_endline("O")
  		  else print_endline("X") /*X*/
  */
  /*--------------test case---------------------*/
  /*
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