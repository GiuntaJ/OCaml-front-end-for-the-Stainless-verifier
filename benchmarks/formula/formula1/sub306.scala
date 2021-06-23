import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub306 {
  
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
      case NOT(f1) => { not(eval(f1)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(f1, f2) => {
        (eval(f1), eval(f2)) match {
          case (true, false) => { false }
          case (_, _) => { true }
        }
      }
      case LESS(e1, e2) => {
        val _2 = {
          def evExpr(e) = {
            e match {
              case NUM(i1) => { i1 }
              case PLUS(e1, e2) => { evExpr(e1) + evExpr(e2) }
              case MINUS(e1, e2) => { evExpr(e1) - evExpr(e2) }
            }
          }
          if (evExpr(e1) < evExpr(e2)) true else false
        }
      }
    }
  }
  
  /* TESTCASE
  let f = TRUE
  let f1 = NOT f
  let f2 = ANDALSO (f, f1)
  let f3 = ORELSE (f2, f1)
  let f4 = IMPLY (f, f2)
  let e = NUM 5
  let e1 = NUM 9
  let e2 = NUM (-3)
  let e3 = MINUS (e1, e2)
  let e4 = PLUS (e, e3)
  let e5 = PLUS (MINUS (e4, e1), e2)
  let f5 = LESS (e, e5)
  
  let s_of_b a = if a = true then "True" else "False"
  
  let _ = print_endline (s_of_b (eval f4))
  let _ = print_endline (s_of_b (eval f5))
  */
}