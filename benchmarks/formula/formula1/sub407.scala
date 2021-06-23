import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub407 {
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
  
  
  def toInt: Expr => Int63 = (
    (x) =>
      {
        x match {
          case NUM(i) => { i }
          case PLUS(x, y) => {
            val _6 = {
              val x_0 = toInt(x)
              val _7 = {
                val y_0 = toInt(y)
                x_0 + y_0
              }
            }
          }
          case MINUS(x, y) => {
            val _2 = {
              val x_0 = toInt(x)
              val _3 = {
                val y_0 = toInt(y)
                x_0 - y_0
              }
            }
          }
        }
    }
  )
    
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f_0) => { not(eval(f_0)) }
          case ANDALSO(x, y) => { eval(x) && eval(y) }
          case ORELSE(x, y) => { eval(x) || eval(y) }
          case IMPLY(x, y) => {
            val _14 = {
              val cond = eval(x)
              if (cond == true) eval(y) else true
            }
          }
          case LESS(x, y) => {
            val _10 = {
              val x_0 = toInt(x)
              val _11 = {
                val y_0 = toInt(y)
                x_0 < y_0
              }
            }
          }
        }
    }
  )
    
    
}