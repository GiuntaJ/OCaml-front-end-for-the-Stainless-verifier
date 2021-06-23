import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub202 {
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
  
  def expr_eval: Expr => Int63 = (
    (ex) =>
      {
        ex match {
          case NUM(n) => { n }
          case PLUS(ln, rn) => { expr_eval(ln) + expr_eval(rn) }
          case MINUS(ln, rn) => { expr_eval(ln) - expr_eval(rn) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (fo) =>
      {
        fo match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => { not(eval(f)) }
          case ANDALSO(lf, rf) => { eval(lf) && eval(rf) }
          case ORELSE(lf, rf) => { eval(lf) || eval(rf) }
          case IMPLY(lf, rf) => { not(eval(lf)) || eval(rf) }
          case LESS(ln, rn) => { expr_eval(ln) < expr_eval(rn) }
        }
    }
  )
}