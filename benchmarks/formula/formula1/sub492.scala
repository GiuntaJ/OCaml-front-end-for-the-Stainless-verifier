import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub492 {
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
  
  def exp_to_int(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(i1, i2) => { exp_to_int(i1) + exp_to_int(i2) }
      case MINUS(i1, i2) => { exp_to_int(i1) - exp_to_int(i2) }
    }
  }
  
  
  def eval(fm: Formula): Boolean = {
    fm match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fm2) => { if (eval(fm2)) false else true }
      case ANDALSO(fm1, fm2) => { if (eval(fm1) && eval(fm2)) true else false }
      case ORELSE(fm1, fm2) => { if (eval(fm1) || eval(fm2)) true else false }
      case IMPLY(fm1, fm2) => { if (not(eval(fm1)) || eval(fm2)) true else false
      }
      case LESS(exp1, exp2) => {
        if (exp_to_int(exp1) < exp_to_int(exp2)) true else false
      }
    }
  }
  
}
