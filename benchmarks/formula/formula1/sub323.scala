import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub323 {
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
          case NOT(e) => { not(eval(e)) }
          case ANDALSO(f, s) => { eval(f) && eval(s) }
          case ORELSE(f, s) => { eval(f) || eval(s) }
          case IMPLY(f, s) => { not(eval(f)) || eval(s) }
          case LESS(f, s) => {
            val _2 = {
              def expr(e) = {
                e match {
                  case NUM(i) => { i }
                  case PLUS(f, s) => { expr(f) + expr(s) }
                  case MINUS(f, s) => { expr(f) - expr(s) }
                }
              }
              expr(f) < expr(s)
            }
          }
        }
    }
  )
}