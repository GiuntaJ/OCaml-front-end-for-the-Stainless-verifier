import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub432 {
  /* 2012-11230 Kim sangmin */
  
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
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        val _4 = {
          def eval_expr: Expr => Int63 = (
            (exp) =>
              {
                exp match {
                  case NUM(i) => { i }
                  case PLUS(i, j) => { eval_expr(i) + eval_expr(j) }
                  case MINUS(i, j) => { eval_expr(i) - eval_expr(j) }
                }
            }
          )
          form match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(i) => { not(eval(i)) }
            case ANDALSO(i, j) => { eval(i) && eval(j) }
            case ORELSE(i, j) => { eval(i) || eval(j) }
            case IMPLY(i, j) => {
              
                if (
                  not(eval(i))
                ) {
                  true 
                } else if (
                  eval(j)
                ) {
                  true 
                } else {
                  false
                }
            }
            case LESS(i, j) => {
              if (eval_expr(i) < eval_expr(j)) true else false
            }
          }
        }
    }
  )
  
}
