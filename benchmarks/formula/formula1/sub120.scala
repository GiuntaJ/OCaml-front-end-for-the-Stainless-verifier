import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub120 {
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
  
  def eval(fml: Formula): Boolean = {
    val _2 = {
      def get(exp) = {
        exp match {
          case NUM(i) => { i }
          case PLUS(exp1, exp2) => { get(exp1) + get(exp2) }
          case MINUS(exp1, exp2) => { get(exp1) - get(exp2) }
        }
      }
      fml match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(fml1) => { not(eval(fml1)) }
        case ANDALSO(fml1, fml2) => { eval(fml1) && eval(fml2) }
        case ORELSE(fml1, fml2) => { eval(fml1) || eval(fml2) }
        case IMPLY(fml1, fml2) => {
          
            if (
              eval(fml2)
            ) {
              true 
            } else if (
              eval(fml1)
            ) {
              false 
            } else {
              true
            }
        }
        case LESS(exp1, exp2) => { get(exp1) < get(exp2) }
      }
    }
  }
}