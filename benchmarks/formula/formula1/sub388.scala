import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub388 {
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
  
  def calc_expr(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(ex1, ex2) => { calc_expr(ex1) + calc_expr(ex2) }
      case MINUS(ex1, ex2) => { calc_expr(ex1) - calc_expr(ex2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(g) => { if (eval(g)) false else true }
      case ANDALSO(g, h) => {
        val _11 = {
          val ge = eval(g)
          val he = eval(h)
          if (ge && he) true else false
        }
      }
      case ORELSE(g, h) => {
        val _8 = {
          val ge = eval(g)
          val he = eval(h)
          if (ge || he) true else false
        }
      }
      case IMPLY(g, h) => {
        val _5 = {
          val ge = eval(g)
          val he = eval(h)
          if (ge && not(he)) false else true
        }
      }
      case LESS(e1, e2) => {
        val _2 = {
          val ev1 = calc_expr(e1)
          val ev2 = calc_expr(e2)
          if (ev1 < ev2) true else false
        }
      }
    }
  }
}