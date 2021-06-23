import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub314 {
  /* C:\Users\saigoy\Desktop\eval.ml */
  
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
    (fml) =>
      {
        val _4 = {
          def calc(exp) = {
            exp match {
              case NUM(n) => { n }
              case PLUS(le, re) => { calc(le) + calc(re) }
              case MINUS(le, re) => { calc(le) - calc(re) }
            }
          }
          fml match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(f) => { eval(f) }
            case ANDALSO(lf, rf) => { eval(lf) && eval(rf) }
            case ORELSE(lf, rf) => { eval(lf) || eval(rf) }
            case IMPLY(lf, rf) => { if (eval(lf)) eval(rf) else true }
            case LESS(le, re) => { calc(le) < calc(re) }
          }
        }
    }
  )
}
