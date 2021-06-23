import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub111 {
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
    val _2 = {
      def solexpr(expr) = {
        expr match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { solexpr(e1) + solexpr(e2) }
          case MINUS(e1, e2) => { solexpr(e1) - solexpr(e2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { not(eval(a)) }
        case ANDALSO(a, b) => { eval(a) && eval(b) }
        case ORELSE(a, b) => { eval(a) || eval(b) }
        case IMPLY(a, b) => { if (eval(a) == false) true else eval(a) && eval(b)
        }
        case LESS(e1, e2) => { if (solexpr(e1) < solexpr(e2)) true else false }
      }
    }
  }
}