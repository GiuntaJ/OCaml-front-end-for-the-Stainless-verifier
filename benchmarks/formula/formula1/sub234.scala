import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub234 {
  /*컴퓨터공학부 2010-11779 박진영 1.2*/
  sealed case class Rec_exception() extends Exception {}
  
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
  
  def eval(fm: Formula): Boolean = {
    val _2 = {
      def cal(e) = {
        e match {
          case NUM(i) => { e }
          case PLUS(e1, e2) => {
            (cal(e1), cal(e2)) match {
              case (NUM(i1), NUM(i2)) => { NUM(i1 + i2) }
              case _ => { assert(false, "Rec_exception") }
            }
          }
          case MINUS(e1, e2) => {
            (cal(e1), cal(e2)) match {
              case (NUM(i1), NUM(i2)) => { NUM(i1 - i2) }
              case _ => { assert(false, "Rec_exception") }
            }
          }
        }
      }
      val _3 = {
        def evalfunc(fm) = {
          fm match {
            case TRUE => { TRUE }
            case FALSE => { FALSE }
            case NOT(f) => {
              evalfunc(f) match {
                case TRUE => { FALSE }
                case FALSE => { TRUE }
                case _ => { assert(false, "Rec_exception") }
              }
            }
            case ANDALSO(f1, f2) => {
              (evalfunc(f1), evalfunc(f2)) match {
                case (TRUE, TRUE) => { TRUE }
                case _ => { FALSE }
              }
            }
            case ORELSE(f1, f2) => {
              (evalfunc(f1), evalfunc(f2)) match {
                case (FALSE, FALSE) => { FALSE }
                case _ => { TRUE }
              }
            }
            case IMPLY(f1, f2) => {
              (evalfunc(f1), evalfunc(f2)) match {
                case (TRUE, FALSE) => { FALSE }
                case _ => { TRUE }
              }
            }
            case LESS(e1, e2) => {
              (cal(e1), cal(e2)) match {
                case (NUM(i1), NUM(i2)) => { if (i1 < i2) TRUE else FALSE }
                case _ => { assert(false, "Rec_exception") }
              }
            }
          }
        }
        evalfunc(fm) match {
          case TRUE => { true }
          case FALSE => { false }
          case _ => { assert(false, "Rec_exception") }
        }
      }
    }
  }
}