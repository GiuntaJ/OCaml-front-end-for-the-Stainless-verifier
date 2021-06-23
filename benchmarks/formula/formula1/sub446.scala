import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub446 {
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
  
  def eval(x: Formula): Boolean = {
    x match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(g, h) => { eval(g) && eval(h) }
      case ORELSE(i, j) => { eval(i) || eval(j) }
      case IMPLY(k, l) => { if (eval(k)) eval(l) else true }
      case LESS(c, d) => {
        val _2 = {
          def calculator(inputExpr: Expr): Int63 = {
            inputExpr match {
              case NUM(i) => { i }
              case PLUS(e1, e2) => { calculator(e1) + calculator(e2) }
              case MINUS(e3, e4) => { calculator(e3) - calculator(e4) }
            }
          }
          calculator(c) < calculator(d)
        }
      }
    }
  }
}