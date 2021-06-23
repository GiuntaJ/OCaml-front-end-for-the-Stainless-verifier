import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub444 {
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
  
  def eval_e: Expr => Int63 = (
    (e) =>
      {
        e match {
          case NUM(n) => { n }
          case PLUS(n1, n2) => { eval_e(n1) + eval_e(n2) }
          case MINUS(n1, n2) => { eval_e(n1) - eval_e(n2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(n) => { not(eval(n)) }
          case ANDALSO(n1, n2) => { eval(n1) && eval(n2) }
          case ORELSE(n1, n2) => { eval(n1) || eval(n2) }
          case IMPLY(n1, n2) => { not(eval(n1)) || eval(n1) && eval(n2) }
          case LESS(e1, e2) => { eval_e(MINUS(e1, e2)) < 0 }
        }
    }
  )
}