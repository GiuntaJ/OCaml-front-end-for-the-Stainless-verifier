import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub412 {
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
  
  def expreval: Expr => Int63 = (
    (e) =>
      {
        e match {
          case NUM(ex) => { ex }
          case PLUS(ex, ey) => { expreval(ex) + expreval(ey) }
          case MINUS(ex, ey) => { expreval(ex) - expreval(ey) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(fx) => { if (eval(fx)) false else true }
          case ANDALSO(fx, fy) => { eval(fx) && eval(fy) }
          case ORELSE(fx, fy) => { eval(fx) || eval(fy) }
          case IMPLY(fx, fy) => {
            eval(fx) match {
              case true => { if (eval(fy)) true else false }
              case false => { true }
            }
          }
          case LESS(ex, ey) => { expreval(ex) < expreval(ey) }
        }
    }
  )
  
  /* TESTING FIELD BELOW */
}
