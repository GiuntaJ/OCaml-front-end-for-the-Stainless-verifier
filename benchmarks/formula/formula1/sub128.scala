import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub128 {
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
  
  
  def eval(fm: Formula): Boolean = {
    val _2 = {
      def cal(exp) = {
        exp match {
          case NUM(i) => { i }
          case PLUS(a, b) => { cal(a) + cal(b) }
          case MINUS(a, b) => { cal(a) - cal(b) }
        }
      }
      fm match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { if (eval(x) == true) false else true }
        case ANDALSO(x, y) => {
          if (eval(x) == true && eval(y) == true) true else false
        }
        case ORELSE(x, y) => {
          if (eval(x) == true || eval(y) == true) true else false
        }
        case IMPLY(x, y) => {
          
            if (
              eval(x) == true && eval(y) == true || eval(x) == false
            ) {
              true 
            } else {
              false
            }
        }
        case LESS(exp1, exp2) => { if (cal(exp1) < cal(exp2)) true else false }
      }
    }
  }
}
