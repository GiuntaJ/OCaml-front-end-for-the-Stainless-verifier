import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub119 {
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
  
  def calExpr(exp: Expr): Int63 = {
    exp match {
      case NUM(i) => { i }
      case PLUS(left, right) => { calExpr(left) + calExpr(right) }
      case MINUS(left, right) => { calExpr(left) - calExpr(right) }
    }
  }
  
  def eval(fml: Formula): Boolean = {
    fml match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(subFml) => { not(eval(subFml)) }
      case ANDALSO(fml1, fml2) => { eval(fml1) && eval(fml2) }
      case ORELSE(fml1, fml2) => { eval(fml1) || eval(fml2) }
      case IMPLY(fml1, fml2) => {
        if (eval(fml1) && eval(fml2) || not(eval(fml1))) true else false
      }
      case LESS(fml1, fml2) => {
        if (calExpr(fml1) < calExpr(fml2)) true else false
      }
    }
  } 
}