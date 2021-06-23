import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub241 {
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
  
  def not_formula(a: Boolean): Boolean = {
    a match {
      case true => { false }
      case false => { true }
    }
  }
  
  def andalso_formula(((a, b))) = { if (a == true && b == true) true else false
  }
  
  def orelse_formula(((a, b))) = { if (a == true || b == true) true else false }
  
  def imply_formula(((a, b))) = { orelse_formula(not_formula(a), b) }
  
  def less_formula(((a, b))) = { if (a - b < 0) true else false }
  
  def calc(expr: Expr): Int63 = {
    expr match {
      case NUM(a) => { a }
      case PLUS(a, b) => { calc(a) + calc(b) }
      case MINUS(a, b) => { calc(a) - calc(b) }
    }
  } 
  
  def eval(formula: Formula): Boolean = {
    formula match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => { not_formula(eval(a)) }
      case ANDALSO(a, b) => { andalso_formula(eval(a), eval(b)) }
      case ORELSE(a, b) => { orelse_formula(eval(a), eval(b)) }
      case IMPLY(a, b) => { imply_formula(eval(a), eval(b)) }
      case LESS(a, b) => { less_formula(calc(a), calc(b)) }
    }
  }
}