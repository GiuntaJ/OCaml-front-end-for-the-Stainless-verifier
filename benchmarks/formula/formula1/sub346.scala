import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub346 {
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
  
  def val_of_expr: Expr => Int63 = (
    x =>
      x match {
        case NUM(v) => { v }
        case PLUS(e1, e2) => {
          val _9 = {
            val v1 = val_of_expr(e1)
            val _10 = {
              val v2 = val_of_expr(e2)
              v1 + v2
            }
          }
        }
        case MINUS(e1, e2) => {
          val _5 = {
            val v1 = val_of_expr(e1)
            val _6 = {
              val v2 = val_of_expr(e2)
              v1 - v2
            }
          }
        }
      }
  )    
  
  def eval: Formula => Boolean = (
    x =>
      x match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(p) => { not(eval(p)) }
        case ANDALSO(p1, p2) => { eval(p1) && eval(p2) }
        case ORELSE(p1, p2) => { eval(p1) || eval(p2) }
        case IMPLY(p1, p2) => {
          val _20 = {
            val b1 = eval(p1)
            val _21 = {
              val b2 = eval(p2)
              not(b1) || b2
            }
          }
        }
        case LESS(e1, e2) => {
          val _16 = {
            val v1 = val_of_expr(e1)
            val _17 = {
              val v2 = val_of_expr(e2)
              v1 < v2
            }
          }
        }
      }
  ) 
}