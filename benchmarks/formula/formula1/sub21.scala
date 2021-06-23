import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub21 {
  /*2006 11720 2-3 KimEunSol*/
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
  
  def eval_exp(e) = {
    e match {
      case NUM(a) => { a }
      case PLUS(a, b) => { eval_exp(a) + eval_exp(b) }
      case MINUS(a, b) => { eval_exp(a) - eval_exp(b) }
    }
  }
  def eval(a) = {
    a match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(b) => { if (eval(b) == true) false else true }
      case ANDALSO(b, c) => { eval(b) && eval(c) }
      case ORELSE(b, c) => { eval(b) || eval(c) }
      case IMPLY(b, c) => {
        
          if (
            eval(b) == true
          ) {
            true 
          } else if (
            eval(c) == false
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(b, c) => { if (eval_exp(b) < eval_exp(b)) true else false }
    }
  }
  
}
