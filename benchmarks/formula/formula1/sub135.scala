import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub135 {
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
  def eval_expr(e: Expr): Int63 = {
    e match {
      case NUM(x) => { x }
      case PLUS(e_1, e_2) => { eval_expr(e_1) + eval_expr(e_2) }
      case MINUS(e_1, e_2) => { eval_expr(e_1) - eval_expr(e_2) }
    }
  }
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f_) => { not(eval(f_)) }
      case ANDALSO(f_1, f_2) => { eval(f_1) && eval(f_2) }
      case ORELSE(f_1, f_2) => { eval(f_1) || eval(f_2) }
      case IMPLY(f_1, f_2) => { eval(ANDALSO(f_1, f_2)) || eval(NOT(f_1)) }
      case LESS(e_1, e_2) => { eval_expr(e_1) < eval_expr(e_2) }
    }
  }
  	
  /* TEST SET*/
  /*
  let _ =
      print_string "HW6 Test Set
  ";
      Printf.printf ("%b") (eval (NOT TRUE));
      print_newline ();
      Printf.printf ("%b") (eval (ANDALSO(LESS(NUM 10, NUM 5), LESS(PLUS (NUM 4, NUM 5) , MINUS (NUM 50, NUM 1)))));
      print_newline ();
      Printf.printf ("%b") (eval (NOT (ANDALSO (LESS (NUM 0, NUM 0), TRUE))));
      print_newline ();
      Printf.printf ("%b") (eval (IMPLY(LESS (NUM 0, NUM 1), ANDALSO(LESS (NUM 0, NUM 0), NOT TRUE))));
      print_newline ();
      Printf.printf ("%b") (eval (IMPLY(LESS (NUM 1, NUM 0), ORELSE(ANDALSO(TRUE, FALSE), ORELSE(NOT TRUE, LESS(NUM 1, NUM 2))))));
      print_newline ()
  */
}