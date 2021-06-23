import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub62 {
  /* C:\Users\owner\Desktop\Homework 1(5).ml */
  
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
      def cal(exp) = {
        exp match {
          case NUM(ex1) => { ex1 }
          case PLUS(ex1, ex2) => { cal(ex1) + cal(ex2) }
          case MINUS(ex1, ex2) => { cal(ex1) - cal(ex2) }
        }
      }
      form match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(for1) => { not(eval(for1)) }
        case ANDALSO(for1, for2) => { eval(for1) && eval(for2) }
        case ORELSE(for1, for2) => { eval(for1) || eval(for2) }
        case IMPLY(for1, for2) => { eval(for1) eq eval(for2) }
        case LESS(e1, e2) => { cal(e1) < cal(e2) }
      }
    }
  }
}
