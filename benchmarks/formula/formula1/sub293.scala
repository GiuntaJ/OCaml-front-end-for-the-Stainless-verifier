import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub293 {
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
  
  def eval(formula: Formula): Boolean = {
    val _2 = {
      def solve(expr) = {
        expr match {
          case NUM(a) => { a }
          case PLUS(a, b) => { solve(a) + solve(b) }
          case MINUS(a, b) => { solve(a) - solve(b) }
        }
      }
      formula match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { if (eval(a) == true) false else true }
        case ANDALSO(a, b) => {
          if (eval(a) == true && eval(b) == true) true else false
        }
        case ORELSE(a, b) => {
          if (eval(a) == false && eval(b) == false) false else true
        }
        case IMPLY(a, b) => {
          
            if (
              eval(a) == false
            ) {
              true 
            } else if (
              eval(b) == true
            ) {
              true 
            } else {
              false
            }
        }
        case LESS(a, b) => { if (solve(a) < solve(b)) true else false }
      }
    }
  } 
}