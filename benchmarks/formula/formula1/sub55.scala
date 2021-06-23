import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub55 {
  /* 4190.310 Programming Language			*
   * Homework #1 - Exercise 5 (참거짓)		*
   * 2008-11744 Jongwook Choi 				*/
  
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
      def evalexpr(e) = {
        e match {
          case NUM(q) => { q }
          case PLUS(f, g) => { evalexpr(f) + evalexpr(g) }
          case MINUS(f, g) => { evalexpr(f) - evalexpr(g) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f_0) => { not(eval(f_0)) }
        case ANDALSO(f_0, g_0) => { eval(f_0) && eval(g_0) }
        case ORELSE(f_0, g_0) => { eval(f_0) || eval(g_0) }
        case IMPLY(f_0, g_0) => { not(eval(f_0)) || eval(g_0) }
        case LESS(e1, e2) => { evalexpr(e1) < evalexpr(e2) }
      }
    }
  }
}
