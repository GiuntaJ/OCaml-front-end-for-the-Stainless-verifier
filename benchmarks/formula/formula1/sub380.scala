import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub380 {
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
  
  def getnum(e: Expr): Int63 = {
    e match {
      case NUM(a) => { a }
      case PLUS(a, b) => { getnum(a) + getnum(b) }
      case MINUS(a, b) => { getnum(a) - getnum(b) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { not(eval(a)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case IMPLY(a, b) => {
        if (eval(a) == true && eval(b) == false) false else true
      }
      case LESS(a, b) => { getnum(a) < getnum(b) }
    }
  }
  
  /*let a61 = eval TRUE
  let a62 = eval FALSE
  let a63 = eval (NOT TRUE)
  let a64 = eval (ANDALSO (TRUE, FALSE))
  let a65 = eval (ORELSE (TRUE, FALSE))
  let a66 = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8))) 
  
  let _ = print_endline(string_of_bool a61)
  let _ = print_endline(string_of_bool a62)
  let _ = print_endline(string_of_bool a63)
  let _ = print_endline(string_of_bool a64)
  let _ = print_endline(string_of_bool a65)
  let _ = print_endline(string_of_bool a66)*/
}