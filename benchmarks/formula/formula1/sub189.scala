import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub189 {
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
  
  def calc(ex: Expr): Int63 = {
    ex match {
      case NUM(f) => { f }
      case PLUS(a, b) => { calc(a) + calc(b) }
      case MINUS(a, b) => { calc(a) - calc(b) }
    }
  }
  
  
  def eval(e: Formula): Boolean = {
    e match {
      case NOT(x) => { ( (x) => { if (x eq TRUE) false else true } )(x) }
      case ANDALSO(a, b) => {
        
          if (
            a eq FALSE
          ) {
            false 
          } else if (
            b eq FALSE
          ) {
            false 
          } else {
            true
          }
      }
      case ORELSE(a, b) => {
        
          if (
            a eq TRUE
          ) {
            true 
          } else if (
            b eq TRUE
          ) {
            true 
          } else {
            false
          }
      }
      case IMPLY(a, b) => { if (a eq FALSE && b eq TRUE) false else true }
      case LESS(a, b) => { calc(a) < calc(b) }
      case FALSE => { false }
      case TRUE => { true }
    }
  }
  
  
  /*
  let _ = if (eval( LESS( NUM(5), PLUS(NUM(10),NUM(2)) ) ) == false) then print_string("test")
  */
}