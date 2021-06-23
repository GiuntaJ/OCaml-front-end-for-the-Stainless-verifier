import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub226 {
  /*2009-11718 박준상 1-2*/
  
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
  
  
  def eval(form: Formula): Boolean = {
    val _2 = {
      def calc(expr) = {
        expr match {
          case NUM(a) => { a }
          case PLUS(exp1, exp2) => { calc(exp1) + calc(exp2) }
          case MINUS(exp1, exp2) => { calc(exp1) - calc(exp2) }
        }
      }
      form match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { not(eval(f)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => {
          if (eval(f1) == true && eval(f2) == false) false else true
        }
        case LESS(ex1, ex2) => { if (calc(ex1) < calc(ex2)) true else false }
      }
    }
  }
}