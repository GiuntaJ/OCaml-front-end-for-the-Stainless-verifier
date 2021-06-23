import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub170 {
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
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(g) => { not(eval(g)) }
      case ANDALSO(g, h) => { eval(g) && eval(h) }
      case ORELSE(g, h) => { eval(g) || eval(h) }
      case IMPLY(g, h) => { not(eval(g)) || eval(h) }
      case LESS(d, e) => {
        val _2 = {
          def eval_expr(e: Expr): Int63 = {
            e match {
              case NUM(a) => { a }
              case PLUS(b, c) => { eval_expr(b) + eval_expr(c) }
              case MINUS(b, c) => { eval_expr(b) - eval_expr(c) }
            }
          }
          val _3 = {
            val x: Int63 = eval_expr(d)
            val _4 = {
              val y: Int63 = eval_expr(e)
              if (x < y) true else false
            }
          }
        }
      }
    }
  }
}