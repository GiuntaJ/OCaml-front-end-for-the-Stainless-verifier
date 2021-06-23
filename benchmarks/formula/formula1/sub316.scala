import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub316 {
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
  
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(subForm) => { not(eval(subForm)) }
      case ANDALSO(subForm1, subForm2) => { eval(subForm1) && eval(subForm2) }
      case ORELSE(subForm1, subForm2) => { eval(subForm1) || eval(subForm2) }
      case IMPLY(subForm1, subForm2) => { not(eval(subForm1)) || eval(subForm2)
      }
      case LESS(subExpr1, subExpr2) => {
        val _2 = {
          def calc(e) = {
            e match {
              case NUM(n) => { n }
              case PLUS(subExpr1, subExpr2) => { calc(subExpr1) + calc(subExpr2)
              }
              case MINUS(subExpr1, subExpr2) => {
                calc(subExpr1) - calc(subExpr2)
              }
            }
          }
          calc(subExpr1) < calc(subExpr2)
        }
      }
    }
  }
}