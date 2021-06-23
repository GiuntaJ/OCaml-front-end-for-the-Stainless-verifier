import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub335 {
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
          case PLUS(exp1, exp2) => { calc(exp1) + calc(exp2) }
          case MINUS(exp1, exp2) => { calc(exp1) - calc(exp2) }
        }
    }
  )
  
  
  def eval: Formula => Boolean = (
    (formula) =>
      {
        formula match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(form1) => { not(eval(form1)) }
          case ANDALSO(form1, form2) => { eval(form1) && eval(form2) }
          case ORELSE(form1, form2) => { eval(form1) || eval(form2) }
          case IMPLY(form1, form2) => { not(eval(form1)) || eval(form2) }
          case LESS(exp1, exp2) => {
            if (calc(exp1) < calc(exp2)) true else false
          }
        }
    }
  )
}