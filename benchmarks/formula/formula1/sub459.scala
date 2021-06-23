import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub459 {
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
  
  def value: Expr => Int63 = (
    (a) =>
      {
        a match {
          case NUM(a) => { a }
          case PLUS(a, b) => { value(a) + value(b) }
          case MINUS(a, b) => { value(a) - value(b) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => { if (a == TRUE) false else true }
          case ANDALSO(a, b) => {
            
              if (
                a == FALSE
              ) {
                false 
              } else if (
                b == FALSE
              ) {
                false 
              } else {
                true
              }
          }
          case ORELSE(a, b) => {
            
              if (
                a == TRUE
              ) {
                true 
              } else if (
                b == TRUE
              ) {
                true 
              } else {
                false
              }
          }
          case IMPLY(a, b) => {
            
              if (
                a == FALSE
              ) {
                true 
              } else if (
                b == TRUE
              ) {
                true 
              } else {
                false
              }
          }
          case LESS(a, b) => { if (value(a) < value(b)) true else false }
        }
    }
  )
}