import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub248 {
  
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
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(ea, eb) => { calc(ea) + calc(eb) }
      case MINUS(ea, eb) => { calc(ea) - calc(eb) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fa) => { not(eval(fa)) }
      case ANDALSO(fa, fb) => {
        eval(fa) match {
          case true => { eval(fb) }
          case _ => { false }
        }
      }
      case ORELSE(fa, fb) => {
        eval(fa) match {
          case true => { true }
          case _ => { eval(fb) }
        }
      }
      case IMPLY(fa, fb) => {
        eval(fa) match {
          case true => { eval(fb) }
          case _ => { true }
        }
      }
      case LESS(ea, eb) => {
        compare(calc(ea), calc(eb)) match {
          case -1 => { true }
          case _ => { false }
        }
      }
    }
  }
}