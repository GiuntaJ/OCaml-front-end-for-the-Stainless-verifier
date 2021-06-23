import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub416 {
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
      def cal(exp) = {
        exp match {
          case NUM(n) => { n }
          case PLUS(exp1, exp2) => { cal(exp1) + cal(exp2) }
          case MINUS(exp1, exp2) => { cal(exp1) - cal(exp2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(p) => { not(eval(p)) }
        case ANDALSO(p, q) => {
          if (eval(p) == true && eval(q) == true) true else false
        }
        case ORELSE(p, q) => {
          if (eval(p) == false && eval(q) == false) false else true
        }
        case IMPLY(p, q) => {
          if (eval(p) == true && eval(q) == false) false else true
        }
        case LESS(exp1, exp2) => { if (cal(exp1) < cal(exp2)) true else false }
      }
    }
  }
}
