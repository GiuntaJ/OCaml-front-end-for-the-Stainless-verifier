import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub352 {
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
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(n, m) => { calc(n) + calc(m) }
      case MINUS(n, m) => { calc(n) - calc(m) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { not(eval(p)) }
      case ANDALSO(p, q) => { eval(p) && eval(q) }
      case ORELSE(p, q) => { eval(p) || eval(q) }
      case IMPLY(p, q) => { not(eval(p)) || eval(q) }
      case LESS(a, b) => { calc(a) < calc(b) }
    }
  }
  
  /*
  let bool_to_string e =
      match e with
      | true -> "true"
      | false -> "false";;
  
  print_endline (bool_to_string (eval TRUE));;
  print_endline (bool_to_string (eval (ORELSE (TRUE, FALSE))));;
  print_endline (bool_to_string (eval (LESS (NUM 10, PLUS(NUM 5, NUM 4)))));;
  */
}