import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub125 {
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
  
  def eval(fm: Formula): Boolean = {
    val _2 = {
      def exprcalc(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(f1, f2) => { exprcalc(f1) + exprcalc(f2) }
          case MINUS(f1, f2) => { exprcalc(f1) - exprcalc(f2) }
        }
      }
      fm match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f) => { not(eval(f)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => {
          if (eval(f1) eq true && eval(f2) eq false) false else true
        }
        case LESS(f1, f2) => { if (exprcalc(f1) < exprcalc(f2)) true else false
        }
      }
    }
  }
  val a61: Boolean = eval(TRUE) 
  val a62: Boolean = eval(FALSE) 
  val a63: Boolean = eval(NOT(TRUE)) 
  val a64: Boolean = eval(ANDALSO(TRUE, FALSE)) 
  val a65: Boolean = eval(ORELSE(TRUE, FALSE)) 
  val a66: Boolean = eval(LESS(PLUS(NUM(3), NUM(4)), MINUS(NUM(7), NUM(8))))
}