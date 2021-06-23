import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub377 {
  /* 2015 - 14718 Giyeon Kim HW 2 */
  
  /* Exercise 1 */
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
  
  def calc: Expr => Int63 = (
    (expr) =>
      {
        expr match {
          case NUM(i) => { i }
          case PLUS(l, r) => { calc(l) + calc(r) }
          case MINUS(l, r) => { calc(l) - calc(r) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        form match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => { not(eval(f)) }
          case ANDALSO(l, r) => { eval(l) && eval(r) }
          case ORELSE(l, r) => { eval(l) || eval(r) }
          case IMPLY(l, r) => { not(eval(l)) || eval(r) }
          case LESS(lexpr, rexpr) => { calc(lexpr) < calc(rexpr) }
        }
    }
  )
  
}
