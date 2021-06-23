import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub10 {
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
    (formula) =>
      {
        val _4 = {
          def temp(expr) = {
            expr match {
              case NUM(a) => { a }
              case PLUS(a, b) => { temp(a) + temp(b) }
              case MINUS(a, b) => { temp(a) - temp(b) }
            }
          }
          formula match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f1) => { if (eval(f1) == true) false else true }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => { eval(f1) && eval(f2) || eval(NOT(f1)) }
            case LESS(e1, e2) => { if (temp(e1) < temp(e2)) true else false }
          }
        }
    }
  )
  
  val a: Formula = TRUE
  val b: Formula = FALSE
  val c: Formula = NOT(TRUE)
  val d: Expr = PLUS(NUM(1), NUM(2))
  val e: Expr = MINUS(NUM(4), NUM(3))
}