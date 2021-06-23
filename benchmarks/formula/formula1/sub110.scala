import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub110 {
  /* HW1 exercise6 2009-11697 Kim HyunJoon */
  /* Eval */
  
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
          def value(exp) = {
            exp match {
              case NUM(n) => { n }
              case PLUS(exp1, exp2) => { value(exp1) + value(exp2) }
              case MINUS(exp1, exp2) => { value(exp1) - value(exp2) }
            }
          }
          input match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f) => { not(eval(f)) }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
            case LESS(exp1, exp2) => { value(exp1) < value(exp2) }
          }
        }
    }
  )
}