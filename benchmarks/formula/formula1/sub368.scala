import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub368 {
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
  
  def eval(form: Formula): Boolean = {
    val _2 = {
      def exprTOINT(expr) = {
        expr match {
          case NUM(a) => { a }
          case PLUS(a, b) => { exprTOINT(a) + exprTOINT(b) }
          case MINUS(a, b) => { exprTOINT(a) - exprTOINT(b) }
        }
      }
      val _3 = {
        def formTOBool(form) = {
          form match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(a) => { not(formTOBool(a)) }
            case ANDALSO(a, b) => { formTOBool(a) && formTOBool(b) }
            case ORELSE(a, b) => { formTOBool(a) || formTOBool(b) }
            case IMPLY(a, b) => {
              
                if (
                  formTOBool(a) eq true && formTOBool(b) eq false
                ) {
                  false 
                } else {
                  true
                }
            }
            case LESS(h, t) => {
              
                if (
                  exprTOINT(h) < exprTOINT(t)
                ) {
                  formTOBool(TRUE) 
                } else {
                  formTOBool(FALSE)
                }
            }
          }
        }
        formTOBool(form)
      }
    }
  }
  	
}