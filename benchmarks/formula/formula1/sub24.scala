import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub24 {
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
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(k) => { if (eval(k) == true) false else true }
      case ANDALSO(a, b) => { if (eval(a) && eval(b)) true else false }
      case ORELSE(a, b) => { if (eval(a) || eval(b)) true else false }
      case IMPLY(a, b) => {
        if (eval(a) == false || eval(b) == true) true else false
      }
      case LESS(a, b) => {
        val _2 = {
          def eval_expr(e) = {
            e match {
              case NUM(i) => { i }
              case PLUS(a, b) => { eval_expr(a) + eval_expr(b) }
              case MINUS(a, b) => { eval_expr(a) - eval_expr(b) }
            }
          }
          if (eval_expr(a) < eval_expr(b)) true else false
        }
      }
    }
  }
  
  eval(LESS(NUM(3), MINUS(NUM(7), NUM(9))))
}