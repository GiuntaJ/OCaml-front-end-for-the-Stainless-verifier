import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub400 {
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
        form match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(form) => { not(eval(form)) }
          case ANDALSO(f1, f2) => { if (eval(f1) && eval(f2)) true else false }
          case ORELSE(f1, f2) => { if (eval(f1) || eval(f2)) true else false }
          case IMPLY(f1, f2) => { if (eval(f1) && not(eval(f2))) false else true
          }
          case LESS(e1, e2) => { if (calc(e1) < calc(e2)) true else false }
        }
    }
  )
  def calc: Expr => Int63 = (
    (ex) =>
      {
        ex match {
          case NUM(integr) => { integr }
          case PLUS(e1, e2) => { calc(e1) + calc(e2) }
          case MINUS(e1, e2) => { calc(e1) - calc(e2) }
        }
    }
  )
}