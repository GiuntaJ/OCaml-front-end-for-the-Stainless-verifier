import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub139 {
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
  
  def eval: Formula => Boolean = (
    (x) =>
      {
        val _4 = {
          def evalin(x) = {
            x match {
              case PLUS(a, b) => { evalin(a) + evalin(b) }
              case MINUS(a, b) => { evalin(a) - evalin(b) }
              case NUM(a) => { a }
            }
          }
          x match {
            case FALSE => { false }
            case TRUE => { true }
            case ANDALSO(a, b) => { eval(a) && eval(b) }
            case ORELSE(a, b) => { eval(a) || eval(b) }
            case IMPLY(a, b) => { eval(a) == false || eval(b) == true }
            case LESS(a, b) => { evalin(a) < evalin(b) }
            case NOT(a) => { eval(a) == false }
          }
        }
    }
  )
}