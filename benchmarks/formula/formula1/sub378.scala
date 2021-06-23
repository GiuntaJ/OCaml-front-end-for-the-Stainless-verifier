import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub378 {
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
  
  def eval_expr: Expr => Int63 = (
    (a) =>
      {
        a match {
          case NUM(b) => { b }
          case PLUS(b, c) => { eval_expr(b) + eval_expr(c) }
          case MINUS(b, c) => { eval_expr(b) - eval_expr(c) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (a) =>
      {
        a match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(b) => { not(eval(b)) }
          case ANDALSO(b, c) => { eval(b) && eval(c) }
          case ORELSE(b, c) => { eval(b) || eval(c) }
          case IMPLY(b, c) => { eval(c) || not(eval(b)) }
          case LESS(b, c) => { eval_expr(b) < eval_expr(c) }
        }
    }
  )
}