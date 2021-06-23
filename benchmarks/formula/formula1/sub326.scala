import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub326 {
  
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
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(l) => { not(eval(l)) }
          case ANDALSO(l, r) => { eval(l) && eval(r) }
          case ORELSE(l, r) => { eval(l) || eval(r) }
          case IMPLY(l, r) => { not(eval(l)) || eval(r) }
          case LESS(l, r) => {
            val _2 = {
              def evalExpr: Expr => Int63 = (
                (e) =>
                  {
                    e match {
                      case NUM(i) => { i }
                      case PLUS(l, r) => { evalExpr(l) + evalExpr(r) }
                      case MINUS(l, r) => { evalExpr(l) - evalExpr(r) }
                    }
                }
              )
              evalExpr(l) < evalExpr(r)
            }
          }
        }
    }
  )
}
