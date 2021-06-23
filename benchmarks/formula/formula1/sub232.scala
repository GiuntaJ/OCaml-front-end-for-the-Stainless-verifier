import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub232 {
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
  
  
  def eval(formula_eq: Formula): Boolean = {
    val _2 = {
      def imply(bool_set) = {
        bool_set match {
          case (true, bool_y) => { bool_y }
          case (false, bool_y) => { true }
        }
      }
      val _3 = {
        def calc(expr) = {
          expr match {
            case NUM(n) => { n }
            case PLUS(expr_1, expr_2) => { calc(expr_1) + calc(expr_2) }
            case MINUS(expr_1, expr_2) => { calc(expr_1) - calc(expr_2) }
          }
        }
        formula_eq match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(formula_1) => { not(eval(formula_1)) }
          case ANDALSO(formula_1, formula_2) => {
            eval(formula_1) && eval(formula_2)
          }
          case ORELSE(formula_1, formula_2) => {
            eval(formula_1) || eval(formula_2)
          }
          case IMPLY(formula_1, formula_2) => {
            imply(eval(formula_1), eval(formula_2))
          }
          case LESS(expr_1, expr_2) => { calc(expr_1) < calc(expr_2) }
        }
      }
    }
  }
}