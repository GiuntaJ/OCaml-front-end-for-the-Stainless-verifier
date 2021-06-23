import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub8 {
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
      def calculate(exp) = {
        exp match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { calculate(e1) + calculate(e2) }
          case MINUS(e1, e2) => { calculate(e1) - calculate(e2) }
        }
      }
      f match {
        case FALSE => { false }
        case TRUE => { true }
        case NOT(f1) => { not(eval(f1)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => { eval(f2) || not(eval(f2)) && not(eval(f1)) }
        case LESS(e1, e2) => {
          if (calculate(e1) < calculate(e2)) true else false
        }
      }
    }
  }
  											
}