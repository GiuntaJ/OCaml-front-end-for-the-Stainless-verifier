import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub420 {
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
      def calc(e: Expr) = {
        e match {
          case NUM(n) => { n }
          case PLUS(a, b) => { calc(a) + calc(b) }
          case MINUS(a, b) => { calc(a) - calc(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f_0) => { not(eval(f_0)) }
        case ANDALSO(f_0, f__0) => { eval(f_0) && eval(f__0) }
        case ORELSE(f_0, f__0) => { eval(f_0) || eval(f__0) }
        case IMPLY(f_0, f__0) => { not(eval(f_0) && not(eval(f__0))) }
        case LESS(e, e_0) => { calc(e) < calc(e_0) }
      }
    }
  }    
}