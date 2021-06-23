import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub348 {
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
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(n) => { n }
      case PLUS(n1, n2) => { calc(n1) + calc(n2) }
      case MINUS(n1, n2) => { calc(n1) - calc(n2) }
    }
  }	
  	
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(sf) => { if (eval(sf)) false else true }
      case ANDALSO(sf1, sf2) => {
        
          if (
            not(eval(sf1))
          ) {
            false 
          } else if (
            not(eval(sf2))
          ) {
            false 
          } else {
            true
          }
      }
      case ORELSE(sf1, sf2) => {
        
          if (
            eval(sf1)
          ) {
            true 
          } else if (
            eval(sf2)
          ) {
            true 
          } else {
            false
          }
      }
      case IMPLY(sf1, sf2) => { if (eval(sf1) && not(eval(sf2))) false else true
      }
      case LESS(ex1, ex2) => { if (calc(ex1) < calc(ex2)) true else false }
    }
  }
}