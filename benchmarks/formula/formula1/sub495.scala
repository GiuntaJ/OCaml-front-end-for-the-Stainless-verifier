import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub495 {
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
  
  def evale: Expr => Int63 = (
    (exp) =>
      {
        exp match {
          case NUM(n) => { n }
          case PLUS(exp1, exp2) => { evale(exp1) + evale(exp2) }
          case MINUS(exp1, exp2) => { evale(exp1) - evale(exp2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f1) => { not(eval(f1)) }
          case ANDALSO(f1, f2) => { if (eval(f1)) eval(f2) else false }
          case ORELSE(f1, f2) => { if (eval(f1)) true else eval(f2) }
          case IMPLY(f1, f2) => { if (eval(f1) && not(eval(f2))) false else true
          }
          case LESS(exp1, exp2) => { evale(exp1) < evale(exp2) }
        }
    }
  )
}