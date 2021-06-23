import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub453 {
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
        val _4 = {
          def eval_expr: Expr => Int63 = (
            (e) =>
              {
                e match {
                  case NUM(a) => { a }
                  case PLUS(b, c) => { eval_expr(b) + eval_expr(c) }
                  case MINUS(d, e) => { eval_expr(d) - eval_expr(e) }
                }
            }
          )
          f match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(a) => { not(eval(a)) }
            case ANDALSO(b, c) => { eval(b) && eval(c) }
            case ORELSE(d, e) => { eval(d) || eval(e) }
            case IMPLY(f, g) => {
              val _7 = {
                val eval_f: Boolean = eval(f)
                not(eval_f) || eval_f && eval(g)
              }
            }
            case LESS(h, i) => { eval_expr(h) < eval_expr(i) }
          }
        }
    }
  )
  
}
