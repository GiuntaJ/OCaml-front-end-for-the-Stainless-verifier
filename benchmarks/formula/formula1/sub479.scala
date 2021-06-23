import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub479 {
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
  
  def arith(f: Expr): Int63 = {
    f match {
      case NUM(n) => { n }
      case PLUS(n, m) => { arith(n) + arith(m) }
      case MINUS(n, m) => { arith(n) - arith(m) }
    }
  }
  
  def eval(form: Formula): Boolean = {
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(p) => { if (eval(p) == true) false else true }
      case ANDALSO(p, q) => {
        if (eval(p) == true) if (eval(q) == true) true else false else false
      }
      case ORELSE(p, q) => {
        
          if (
            eval(p) == true
          ) {
            true 
          } else if (
            eval(q) == true
          ) {
            true 
          } else {
            false
          }
      }
      case IMPLY(p, q) => {
        if (eval(p) == true) if (eval(q) == true) true else false else true
      }
      case LESS(n, m) => { if (arith(MINUS(n, m)) < 0) true else false }
    }
  }
  
  
    
  
  
}
