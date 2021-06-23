import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub311 {
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
  
  def eval(formula: Formula): Boolean = {
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case IMPLY(a, b) => { not(eval(a)) || eval(b) }
      case LESS(a, b) => {
        val _2 = {
          def calc(expr) = {
            expr match {
              case NUM(i) => { i }
              case PLUS(e1, e2) => { calc(e1) + calc(e2) }
              case MINUS(e1, e2) => { calc(e1) - calc(e2) }
            }
          }
          calc(a) < calc(b)
        }
      }
    }
  }
  /*
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
  
  print_string "End of Test
  ";;
  */
}