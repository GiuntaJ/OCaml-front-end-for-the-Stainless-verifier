import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub12 {
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
      def chupa(exp) = {
        exp match {
          case NUM(i) => { i }
          case PLUS(ex1, ex2) => { chupa(ex1) + chupa(ex2) }
          case MINUS(ex1, ex2) => { chupa(ex1) - chupa(ex2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(forma) => { if (eval(forma) == true) false else true }
        case ANDALSO(forma1, forma2) => {
          if (eval(forma1) == true && eval(forma2) == true) true else false
        }
        case ORELSE(forma1, forma2) => {
          if (eval(forma1) == true || eval(forma2) == true) true else false
        }
        case IMPLY(forma1, forma2) => {
          if (eval(forma1) == true && eval(forma2) == false) false else true
        }
        case LESS(ex1, ex2) => { if (chupa(ex1) < chupa(ex2)) true else false }
      }
    }
  }
}
