import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub338 {
  /*컴공 2014-10618 이세영 1-4*/
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
    x =>
      x match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(x) => { if (eval(x) == true) false else true }
        case ANDALSO(x, y) => {
          if (eval(x) == true && eval(y) == true) true else false
        }
        case ORELSE(x, y) => {
          if (eval(x) == false && eval(y) == false) false else true
        }
        case IMPLY(x, y) => {
          if (eval(x) == true && eval(y) == false) false else true
        }
        case LESS(x, y) => {
          val _5 = {
            def exp(sum) = {
              (
                x =>
                  x match {
                    case NUM(x) => { x }
                    case PLUS(x, y) => { exp(0, x) + exp(0, y) }
                    case MINUS(x, y) => { exp(0, x) - exp(0, y) }
                  }
              )
            }
            if (exp(0, x) < exp(0, y)) true else false
          }
        }
      }
  )
}