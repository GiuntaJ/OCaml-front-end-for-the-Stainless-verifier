import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub88 {
  /* 2009-11824 Jieun-Jeong HW1-5 */
  
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
      def calc(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(el, er) => { calc(el) + calc(er) }
          case MINUS(el, er) => { calc(el) - calc(er) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { if (eval(f)) false else true }
        case ANDALSO(fl, fr) => { eval(fl) && eval(fr) }
        case ORELSE(fl, fr) => { eval(fl) || eval(fr) }
        case IMPLY(fl, fr) => { eval(NOT(fl)) || eval(fr) }
        case LESS(el, er) => { calc(el) < calc(er) }
      }
    }
  }	
}