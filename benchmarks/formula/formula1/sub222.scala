import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub222 {
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
    form match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(first) => { if (eval(first)) false else true }
      case ANDALSO(first, second) => { eval(first) && eval(second) }
      case ORELSE(first, second) => { eval(first) || eval(second) }
      case IMPLY(first, second) => { eval(ORELSE(NOT(first), second)) }
      case LESS(first, second) => {
        val _2 = {
          def eval2(exp) = {
            exp match {
              case NUM(val1) => { val1 }
              case PLUS(val1, val2) => { eval2(val1) + eval2(val2) }
              case MINUS(val1, val2) => { eval2(val1) - eval2(val2) }
            }
          }
          eval2(first) < eval2(second)
        }
      }
    }
  }
  
}
