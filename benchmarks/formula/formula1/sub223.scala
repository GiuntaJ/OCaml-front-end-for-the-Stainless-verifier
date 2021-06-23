import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub223 {
  sealed case class EvalError() extends Exception {}
  
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def eval_expr: Expr => Expr = (
            (e) =>
              {
                e match {
                  case NUM(n) => { NUM(n) }
                  case PLUS(NUM(n1), NUM(n2)) => { NUM(n1 + n2) }
                  case PLUS(e1, e2) => {
                    eval_expr(PLUS(eval_expr(e1), eval_expr(e2)))
                  }
                  case MINUS(NUM(n1), NUM(n2)) => { NUM(n1 - n2) }
                  case MINUS(e1, e2) => {
                    eval_expr(MINUS(eval_expr(e1), eval_expr(e2)))
                  }
                }
            }
          )
          f match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f1) => { not(eval(f1)) }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
            case LESS(e1, e2) => {
              (eval_expr(e1), eval_expr(e2)) match {
                case (NUM(n1), NUM(n2)) => { n1 < n2 }
                case _ => { assert(false, "EvalError") }
              }
            }
          }
        }
    }
  )
}