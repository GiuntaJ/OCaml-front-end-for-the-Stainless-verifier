import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub288 {
  /*
   * Brief      : HW1, Program Language (4190.310)
   * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
   * Student Id : 2014-21767
   * Date       : Sep. 12, 2014
   */
  
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
  
  /* Exercise 2 */
  def eval: Formula => Boolean = val _0 = {
    def eval_expr: Expr => Int63 = (
      x =>
        x match {
          case NUM(n) => { n }
          case PLUS(e1, e2) => { eval_expr(e1) + eval_expr(e2) }
          case MINUS(e1, e2) => { eval_expr(e1) - eval_expr(e2) }
        }
    )
    (
      x =>
        x match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => {
            val _18 = {
              val v = eval(f)
              if (v) false else true
            }
          }
          case ANDALSO(f1, f2) => {
            val _15 = {
              val ((v1, v2)) = (eval(f1), eval(f2))
              if (v1 && v2) true else false
            }
          }
          case ORELSE(f1, f2) => {
            val _12 = {
              val ((v1, v2)) = (eval(f1), eval(f2))
              if (v1 || v2) true else false
            }
          }
          case IMPLY(f1, f2) => {
            val _9 = {
              val ((v1, v2)) = (eval(f1), eval(f2))
              if (v1 && v2 == false) false else true
            }
          }
          case LESS(e1, e2) => {
            val _6 = {
              val ((v1, v2)) = (eval_expr(e1), eval_expr(e2))
              if (v1 < v2) true else false
            }
          }
        }
    )
  }
}