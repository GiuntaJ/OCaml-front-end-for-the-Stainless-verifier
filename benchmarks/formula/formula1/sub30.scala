import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub30 {
  sealed case class Error(param0: String) extends Exception {}
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
  def calc: Expr => Int63 = (
    (myexp) =>
      {
        myexp match {
          case NUM(a) => { a }
          case PLUS(a1, a2) => { calc(a1) + calc(a2) }
          case MINUS(b1, b2) => { calc(b1) - calc(b2) }
        }
    }
  )
  
  def eval_temp: Formula => Boolean = (
    (formu) =>
      {
        formu match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => { not(eval_temp(a)) }
          case ANDALSO(a1, a2) => { eval_temp(a1) && eval_temp(a2) }
          case ORELSE(b1, b2) => { eval_temp(b1) || eval_temp(b2) }
          case IMPLY(c1, c2) => { not(eval_temp(c1) && not(eval_temp(c2))) }
          case LESS(ex1, ex2) => { calc(ex1) < calc(ex2) }
        }
    }
  )
  val eval: Formula => Boolean = ( (formu) => { eval_temp(formu) } )
  		
}