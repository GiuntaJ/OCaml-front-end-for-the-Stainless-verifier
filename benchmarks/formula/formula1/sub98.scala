import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub98 {
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
  
  def eval(fml: Formula): Boolean = {
    fml match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fml_0) => { not(eval(fml_0)) }
      case ANDALSO(fml1, fml2) => { eval(fml1) && eval(fml2) }
      case ORELSE(fml1, fml2) => { eval(fml1) || eval(fml1) }
      case IMPLY(fml1, fml2) => { not(eval(fml1)) || eval(fml2) }
      case LESS(expr1, expr2) => {
        val _2 = {
          def tr01(expr1_0) = {
            expr1_0 match {
              case NUM(i) => { i }
              case MINUS(ex1, ex2) => { tr01(ex1) - tr01(ex2) }
              case PLUS(ex1_0, ex2_0) => { tr01(ex1_0) + tr01(ex2_0) }
            }
          }
          val _3 = {
            def tr02(((expr00, expr01))) = {
              if (tr01(expr00) < tr01(expr01)) true else false
            }
            tr02(expr1, expr2)
          }
        }
      }
    }
  }
  	 
  
}
