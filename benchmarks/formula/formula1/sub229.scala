import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub229 {
  sealed case class TODO() extends Exception {}
  
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
  
  def eval1(f1: Expr): Int63 = {
    f1 match {
      case NUM(n) => { n }
      case PLUS(a, b) => { eval1(a) + eval1(b) }
      case MINUS(a, b) => { eval1(a) - eval1(b) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(n) => { not(eval(n)) }
      case ANDALSO(a, b) => { eval(a) && eval(b) }
      case ORELSE(a, b) => { eval(a) || eval(b) }
      case IMPLY(a, b) => {
        
          if (
            eval(a) && eval(b)
          ) {
            true 
          } else if (
            eval(a) && not(eval(b))
          ) {
            false 
          } else if (
            not(eval(a)) && eval(b)
          ) {
            true 
          } else {
            true
          }
      }
      case LESS(a, b) => { eval1(a) < eval1(b) }
    }
  }
   
}