import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub499 {
  /* HW2 Exercise 1 True or False */
  
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
  
  
  def eval: Formula => Boolean = val _0 = {
    def calc: Expr => Int63 = (
      (e_calculating) =>
        {
          e_calculating match {
            case NUM(number) => { number }
            case PLUS(e1, e2) => { calc(e1) + calc(e2) }
            case MINUS(e1, e2) => { calc(e1) - calc(e2) }
          }
      }
    )
    (
      (f_evaluating) =>
        {
          f_evaluating match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f) => { not(eval(f)) }
            case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
            case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
            case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
            case LESS(e1, e2) => { calc(e1) < calc(e2) }
          }
      }
    )
  }
}