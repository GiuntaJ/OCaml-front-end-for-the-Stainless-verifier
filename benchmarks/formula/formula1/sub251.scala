import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub251 {
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
  
  
  
  def eval: Formula => Boolean = val _0 = {
    def calculate: Expr => Int63 = (
      (exp) =>
        {
          exp match {
            case PLUS(a, b) => { calculate(a) + calculate(b) }
            case MINUS(a, b) => { calculate(a) - calculate(b) }
            case NUM(a) => { a }
          }
      }
    )
    (
      (f) =>
        {
          f match {
            case FALSE => { false }
            case TRUE => { true }
            case ANDALSO(a, b) => { eval(a) && eval(b) }
            case ORELSE(a, b) => { eval(a) || eval(b) }
            case IMPLY(a, b) => { if (eval(a) && not(eval(b))) false else true }
            case LESS(a, b) => {
              if (calculate(a) < calculate(b)) true else false
            }
            case NOT(a) => { not(eval(a)) }
          }
      }
    )
  }
}
