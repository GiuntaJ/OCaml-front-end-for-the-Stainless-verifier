import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub141 {
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
  
   def eval(boo) = {
    val _2 = {
      def oper(x) = {
        x match {
          case NUM(p) => { p }
          case PLUS(x1, x2) => { oper(x1) + oper(x2) }
          case MINUS(x1, x2) => { oper(x1) - oper(x2) }
        }
      }
      boo match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(j) => { not(eval(j)) }
        case ANDALSO(j1, j2) => { eval(j1) && eval(j2) }
        case ORELSE(j1, j2) => { eval(j1) || eval(j2) }
        case IMPLY(j1, j2) => { not(eval(j1)) || eval(j2) }
        case LESS(n1, n2) => { if (oper(n1) < oper(n2)) true else false }
      }
    }
  }
}
