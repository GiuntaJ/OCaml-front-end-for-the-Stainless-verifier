import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub337 {
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def e_eval: Expr => Int63 = (
    (x) =>
      {
        x match {
          case NUM(i) => { i }
          case PLUS(i1, i2) => { e_eval(i1) + e_eval(i2) }
          case MINUS(i1, i2) => { e_eval(i1) - e_eval(i2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (x) =>
      {
        x match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => { not(eval(f)) }
          case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
          case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
          case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
          case LESS(e1, e2) => { e_eval(e1) < e_eval(e2) }
        }
    }
  )
}
