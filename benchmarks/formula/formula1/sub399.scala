import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub399 {
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
        x match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => {
            eval(a) match {
              case true => { false }
              case false => { true }
            }
          }
          case ANDALSO(a, b) => {
            (eval(a), eval(b)) match {
              case (true, true) => { true }
              case (true, false) => { false }
              case (false, _) => { false }
            }
          }
          case ORELSE(a, b) => {
            (eval(a), eval(b)) match {
              case (true, _) => { true }
              case (false, true) => { true }
              case (false, false) => { false }
            }
          }
          case IMPLY(a, b) => {
            (eval(a), eval(b)) match {
              case (true, true) => { true }
              case (true, false) => { false }
              case (false, _) => { true }
            }
          }
          case LESS(x, y) => {
            val _2 = {
              def real: Expr => Int63 = (
                (x) =>
                  {
                    x match {
                      case NUM(i) => { i }
                      case PLUS(k, j) => { real(k) + real(j) }
                      case MINUS(k, j) => { real(k) - real(j) }
                    }
                }
              )
              if (real(x) < real(y)) true else false
            }
          }
        }
    }
  )
}