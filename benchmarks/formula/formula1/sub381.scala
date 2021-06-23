import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub381 {
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
    (input) =>
      {
        val _4 = {
          def eval2: Expr => Int63 = (
            (input) =>
              {
                input match {
                  case NUM(a) => { a }
                  case PLUS(a, b) => { eval2(a) + eval2(b) }
                  case MINUS(a, b) => { eval2(a) - eval2(b) }
                }
            }
          )
          input match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(a) => { not(eval(a)) }
            case ANDALSO(a, b) => { eval(a) && eval(b) }
            case ORELSE(a, b) => { eval(a) || eval(b) }
            case IMPLY(a, b) => { not(eval(a)) || eval(b) }
            case LESS(a, b) => { eval2(a) < eval2(b) }
          }
        }
    }
  )
}