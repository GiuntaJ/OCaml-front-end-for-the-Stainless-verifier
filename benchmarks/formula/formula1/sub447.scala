import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub447 {
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
  
  def calc(a: Expr): Int63 = {
    a match {
      case NUM(a) => { a }
      case PLUS(b, c) => { calc(b) + calc(c) }
      case MINUS(b, c) => { calc(b) - calc(c) }
    }
  }
  
  def eval(input: Formula): Boolean = {
    input match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(a) => {
        eval(a) match {
          case true => { false }
          case false => { true }
        }
      }
      case ANDALSO(a, b) => {
        eval(a) match {
          case true => { eval(b) }
          case false => { false }
        }
      }
      case ORELSE(a, b) => {
        eval(a) match {
          case true => { true }
          case false => { eval(b) }
        }
      }
      case IMPLY(a, b) => {
        eval(a) match {
          case true => { eval(b) }
          case false => { true }
        }
      }
      case LESS(c, d) => { if (calc(c) < calc(d)) true else false }
    }
  }
}
