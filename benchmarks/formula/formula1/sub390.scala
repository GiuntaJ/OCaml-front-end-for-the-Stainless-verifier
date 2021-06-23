import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub390 {
  /*2016-11690*/
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
    (formu) =>
      {
        val _4 = {
          def expr_to_int: Expr => Int63 = (
            (exp) =>
              {
                exp match {
                  case NUM(n) => { n }
                  case PLUS(n1, n2) => { expr_to_int(n1) + expr_to_int(n2) }
                  case MINUS(n1, n2) => { expr_to_int(n1) - expr_to_int(n2) }
                }
            }
          )
          formu match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(form) => { not(eval(form)) }
            case ANDALSO(form1, form2) => {
              if (eval(form1)) eval(form2) else false
            }
            case ORELSE(form1, form2) => {
              if (eval(form1)) true else eval(form2)
            }
            case IMPLY(form1, form2) => { if (eval(form1)) eval(form2) else true
            }
            case LESS(expr1, expr2) => {
              if (expr_to_int(expr1) < expr_to_int(expr2)) true else false
            }
          }
        }
    }
  )
}