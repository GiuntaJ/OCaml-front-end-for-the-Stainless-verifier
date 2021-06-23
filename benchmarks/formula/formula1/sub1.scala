import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub1 {
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
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      def cal(ex: Expr) = {
        ex match {
          case NUM(a) => { a }
          case PLUS(a, b) => { cal(a) + cal(b) }
          case MINUS(a, b) => { cal(a) - cal(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { if (eval(a) == true) false else true }
        case ANDALSO(a, b) => {
          
            if (
              eval(a) == false
            ) {
              false 
            } else if (
              eval(b) == false
            ) {
              false 
            } else {
              true
            }
        }
        case ORELSE(a, b) => {
          
            if (
              eval(a) == true
            ) {
              true 
            } else if (
              eval(b) == true
            ) {
              true 
            } else {
              false
            }
        }
        case IMPLY(a, b) => {
          if (eval(a) == true) if (eval(b) == true) true else false else true
        }
        case LESS(a, b) => { if (cal(a) < cal(b)) true else false }
      }
    }
  }                                      
}