import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub442 {
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
      case NOT(x) => { not(eval(x)) }
      case ANDALSO(x, y) => { eval(x) && eval(y) }
      case ORELSE(x, y) => { eval(x) || eval(y) }
      case IMPLY(x, y) => {
        
          if (
            false == eval(x)
          ) {
            true 
          } else if (
            false == eval(y) && true == eval(x)
          ) {
            false 
          } else {
            true
          }
      }
      case LESS(x, y) => {
        val _2 = {
          def toInt(e: Expr): Int63 = {
            e match {
              case NUM(a) => { a }
              case PLUS(a, b) => { toInt(a) + toInt(b) }
              case MINUS(a, b) => { toInt(a) - toInt(b) }
            }
          }
          val _3 = {
            val ((xInt, yInt)) = (toInt(x), toInt(y))
            if (xInt < yInt) true else false
          }
        }
      }
    }
  }
}