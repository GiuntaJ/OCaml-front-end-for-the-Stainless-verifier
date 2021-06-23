import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub435 {
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
    (f) =>
      {
        val _4 = {
          def cal_expr: Expr => Int63 = (
            (e) =>
              {
                e match {
                  case NUM(i) => { i }
                  case PLUS(e1, e2) => { cal_expr(e1) + cal_expr(e2) }
                  case MINUS(e1, e2) => { cal_expr(e1) - cal_expr(e2) }
                }
            }
          )
          f match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(s) => { not(eval(s)) }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
            case LESS(e1, e2) => {
              if (cal_expr(e1) < cal_expr(e2)) true else false
            }
          }
        }
    }
  )
  
  /*
  let a61 = eval TRUE 
  let a62 = eval FALSE 
  let a63 = eval (NOT TRUE) 
  let a64 = eval (ANDALSO (TRUE, FALSE)) 
  let a65 = eval (ORELSE (TRUE, FALSE)) 
  let a66 = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8)))
  */
}