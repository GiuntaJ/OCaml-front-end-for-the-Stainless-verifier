import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub217 {
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
      def cal(a) = {
        a match {
          case NUM(x) => { x }
          case PLUS(x, y) => { cal(x) + cal(y) }
          case MINUS(x, y) => { cal(x) - cal(y) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(g) => { if (eval(g) == true) false else true }
        case ANDALSO(g, h) => {
          if (eval(g) == true && eval(h) == true) true else false
        }
        case ORELSE(g, h) => {
          if (eval(g) == false && eval(h) == false) false else true
        }
        case IMPLY(g, h) => {
          
            if (
              eval(g) == false
            ) {
              true 
            } else if (
              eval(h) == true
            ) {
              true 
            } else {
              false
            }
        }
        case LESS(g, h) => { if (cal(g) < cal(h)) true else false }
      }
    }
  }
}