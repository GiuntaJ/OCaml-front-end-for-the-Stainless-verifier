import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub137 {
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
  
  def eval(f) = {
    val _2 = {
      def foo(num) = {
        num match {
          case NUM(i) => { i }
          case PLUS(a, b) => { foo(a) + foo(b) }
          case MINUS(a, b) => { foo(a) - foo(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(g) => { not(eval(g)) }
        case ANDALSO(p, q) => { eval(p) && eval(q) }
        case ORELSE(p, q) => { eval(p) || eval(q) }
        case IMPLY(p, q) => { if (eval(p)) eval(q) else true }
        case LESS(a, b) => { foo(a) < foo(b) }
      }
    }
  }
}