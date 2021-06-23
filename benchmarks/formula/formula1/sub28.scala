import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub28 {
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
  
  def eval(p: Formula): Boolean = {
    val _2 = {
      def evalexpr(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(e1, e2) => { evalexpr(e1) + evalexpr(e2) }
          case MINUS(e1, e2) => { evalexpr(e1) - evalexpr(e2) }
        }
      }
      p match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(p1) => { if (eval(p1)) false else true }
        case ANDALSO(p1, p2) => {
          if (eval(p1)) if (eval(p2)) true else false else false
        }
        case ORELSE(p1, p2) => {
          
            if (
              eval(p1)
            ) {
              true 
            } else if (
              eval(p2)
            ) {
              true 
            } else {
              false
            }
        }
        case IMPLY(p1, p2) => {
          if (eval(p1)) if (eval(p2)) true else false else true
        }
        case LESS(e1, e2) => { evalexpr(e1) < evalexpr(e2) }
      }
    }
  }
}