import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub215 {
  
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
    (formula) =>
      {
        val _4 = {
          def expr_eval(expr) = {
            expr match {
              case NUM(i) => { i }
              case PLUS(e1, e2) => { expr_eval(e1) + expr_eval(e2) }
              case MINUS(e1, e2) => { expr_eval(e1) - expr_eval(e2) }
            }
          }
          formula match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f) => { not(eval(f)) }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => {
              val _11 = {
                val f1_res = eval(f1)
                val _12 = {
                  val f2_res = eval(f2)
                  
                    if (
                      f1_res == false
                    ) {
                      true 
                    } else if (
                      f2_res == true
                    ) {
                      true 
                    } else {
                      false
                    }
                }
              }
            }
            case LESS(e1, e2) => {
              val _7 = {
                val e1_res = expr_eval(e1)
                val _8 = {
                  val e2_res = expr_eval(e2)
                  e1_res < e2_res
                }
              }
            }
          }
        }
    }
  )
}