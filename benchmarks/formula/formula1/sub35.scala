import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub35 {
  
  /*Ex5*/
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
  def eval: Formula => Boolean = val _0 = {
    def evale: Expr => Int63 = (
      (ex) =>
        {
          ex match {
            case NUM(a) => { a }
            case PLUS(a, b) => { evale(a) + evale(b) }
            case MINUS(a, b) => { evale(a) - evale(b) }
          }
      }
    )
    (
      (ex) =>
        {
          ex match {
            case TRUE => { true }
            case FALSE => { false }
            case NOT(ex) => { not(eval(ex)) }
            case ANDALSO(a, b) => { eval(a) && eval(b) }
            case ORELSE(a, b) => { eval(a) || eval(b) }
            case IMPLY(a, b) => {
              
                if (
                  eval(b)
                ) {
                  true 
                } else if (
                  eval(a)
                ) {
                  false 
                } else {
                  true
                }
            }
            case LESS(a, b) => { evale(a) < evale(b) }
          }
      }
    )
  }
}