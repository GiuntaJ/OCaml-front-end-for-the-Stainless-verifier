import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub354 {
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
  
  def calculate: Expr => Int63 = (
    (expr) =>
      {
        expr match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { calculate(e1) + calculate(e2) }
          case MINUS(e1, e2) => { calculate(e1) - calculate(e2) }
        }
    }
  )
  def eval: Formula => Boolean = (
    (formula) =>
      {
        formula match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => { not(eval(f)) }
          case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
          case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
          case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
          case LESS(e1, e2) => { calculate(e1) < calculate(e2) }
        }
    }
  )
  /*
  let f1 : formula = ANDALSO(LESS(NUM 3,NUM 4),LESS(PLUS(NUM 1,NUM 2),NUM 1))
  let f2 : formula = ORELSE(FALSE,NOT(LESS(NUM 10,NUM 9)))
  let f3 : formula = IMPLY(f1,f2)
  let b1 : bool = eval(f1)
  let b2 : bool = eval(f2)
  let b3 : bool = eval(f3)
  let _ = print_endline(string_of_bool b1)
  let _ = print_endline(string_of_bool b2)
  let _ = print_endline(string_of_bool b3)
  
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
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
  
  
  */
  
}
