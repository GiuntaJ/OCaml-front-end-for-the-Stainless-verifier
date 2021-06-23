import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub257 {
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
    
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  
  def cal(ex) = {
    ex match {
      case NUM(n) => { n }
      case PLUS(a, b) => { cal(a) + cal(b) }
      case MINUS(a, b) => { cal(a) - cal(b) }
    }
  }
  
  def eval(fm) = {
    fm match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(nf) => { not(eval(nf)) }
      case ANDALSO(fm1, fm2) => { eval(fm1) && eval(fm2) }
      case ORELSE(fm1, fm2) => { eval(fm1) || eval(fm2) }
      case IMPLY(fm1, fm2) => { not(eval(fm1)) || eval(fm2) }
      case LESS(expr1, expr2) => { cal(expr1) < cal(expr2) }
    }
  }
}