import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub213 {
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
  
  
  def exprval(ex: Expr): Int63 = {
    ex match {
      case NUM(n) => { n }
      case PLUS(ex1, ex2) => { exprval(ex1) + exprval(ex2) }
      case MINUS(ex1, ex2) => { exprval(ex1) - exprval(ex2) }
    }
  }
  
  
  
  	
  def eval(fm: Formula): Boolean = {
    fm match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { not(eval(f)) }
      case ANDALSO(fm1, fm2) => { eval(fm1) && eval(fm2) }
      case ORELSE(fm1, fm2) => { eval(fm1) || eval(fm2) }
      case IMPLY(fm1, fm2) => { not(eval(fm1) && not(eval(fm2))) }
      case LESS(exp1, exp2) => {
        if (exprval(exp1) < exprval(exp2)) true else false
      }
    }
  }
}