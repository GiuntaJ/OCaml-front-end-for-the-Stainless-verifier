import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub384 {
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
  
  def plusminus(x: Expr): Int63 = {
    x match {
      case PLUS(a, b) => { plusminus(a) + plusminus(b) }
      case MINUS(a, b) => { plusminus(a) - plusminus(b) }
      case NUM(a) => { a }
    }
  }
  
  
  def eval_help(f: Formula): Formula = {
    f match {
      case FALSE => { FALSE }
      case TRUE => { TRUE }
      case NOT(value) => {
        value match {
          case TRUE => { FALSE }
          case FALSE => { TRUE }
          case _ => { eval_help(NOT(eval_help(value))) }
        }
      }
      case ANDALSO(value, value1) => {
        value match {
          case TRUE => {
            value1 match {
              case TRUE => { TRUE }
              case FALSE => { FALSE }
              case _ => {
                eval_help(ANDALSO(eval_help(value), eval_help(value1)))
              }
            }
          }
          case FALSE => { FALSE }
          case _ => { eval_help(ANDALSO(eval_help(value), eval_help(value1))) }
        }
      }
      case ORELSE(value, value1) => {
        value match {
          case TRUE => { TRUE }
          case FALSE => {
            value1 match {
              case TRUE => { TRUE }
              case FALSE => { FALSE }
              case _ => { eval_help(ORELSE(eval_help(value), eval_help(value1)))
              }
            }
          }
          case _ => { eval_help(ORELSE(eval_help(value), eval_help(value1))) }
        }
      }
      case IMPLY(value, value1) => {
        value match {
          case FALSE => { TRUE }
          case TRUE => {
            value1 match {
              case FALSE => { FALSE }
              case TRUE => { TRUE }
              case _ => { eval_help(IMPLY(eval_help(value), eval_help(value1)))
              }
            }
          }
          case _ => { eval_help(IMPLY(eval_help(value), eval_help(value1))) }
        }
      }
      case LESS(value, value1) => {
        (value, value1) match {
          case (a, b) => { if (plusminus(a) < plusminus(b)) TRUE else FALSE }
        }
      }
    }
  }
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      val returned = eval_help(f)
      returned match {
        case TRUE => { true }
        case FALSE => { false }
      }
    }
  }
  
}
