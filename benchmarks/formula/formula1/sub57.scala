import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub57 {
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
  
  def eval(form: Formula): Boolean = {
    val _2 = {
      def count(x) = {
        x match {
          case NUM(n) => { n }
          case PLUS(a, b) => { count(a) + count(b) }
          case MINUS(a, b) => { count(a) - count(b) }
        }
      }
      form match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { not(eval(f)) }
        case ANDALSO(a, b) => { eval(a) && eval(b) }
        case ORELSE(a, b) => { eval(a) || eval(b) }
        case IMPLY(a, b) => { if (eval(a) && not(eval(b))) false else true }
        case LESS(c, d) => { count(c) < count(d) }
      }
    }
  }
}