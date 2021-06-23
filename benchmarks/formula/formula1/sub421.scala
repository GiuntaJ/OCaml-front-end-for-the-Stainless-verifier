import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub421 {
  sealed abstract class Expr {}
  case class NUM(param0: Int63) extends Expr {}
  case class PLUS(param0: Expr,  param1: Expr) extends Expr {}
  case class MINUS(param0: Expr,  param1: Expr) extends Expr {}
  
  sealed abstract class Formula {}
  case object TRUE extends Formula {}
  case object FALSE extends Formula {}
  case class NOT(param0: Formula) extends Formula {}
  case class ANDALSO(param0: Formula,  param1: Formula) extends Formula {}
  case class ORELSE(param0: Formula,  param1: Formula) extends Formula {}
  case class IMPLY(param0: Formula,  param1: Formula) extends Formula {}
  case class LESS(param0: Expr,  param1: Expr) extends Formula {}
  
  def exprSol: Expr => Expr = (
    (e) =>
      {
        e match {
          case NUM(a) => { NUM(a) }
          case PLUS(p1, p2) => {
            (p1, p2) match {
              case (NUM(a), NUM(b)) => { NUM(a + b) }
              case (a, b) => { exprSol(PLUS(exprSol(a), exprSol(b))) }
            }
          }
          case MINUS(m1, m2) => {
            (m1, m2) match {
              case (NUM(a), NUM(b)) => { NUM(a - b) }
              case (a, b) => { exprSol(MINUS(exprSol(a), exprSol(b))) }
            }
          }
        }
    }
  )
  
  def preEval: Formula => Formula = (
    (form) =>
      {
        form match {
          case TRUE => { TRUE }
          case FALSE => { FALSE }
          case NOT(f) => {
            f match {
              case TRUE => { FALSE }
              case FALSE => { TRUE }
              case a => { preEval(NOT(preEval(f))) }
            }
          }
          case ANDALSO(f1, f2) => {
            (f1, f2) match {
              case (TRUE, TRUE) => { TRUE }
              case (FALSE, b) => { FALSE }
              case (a, FALSE) => { FALSE }
              case (a, b) => { preEval(ANDALSO(preEval(a), preEval(b))) }
            }
          }
          case ORELSE(f1, f2) => {
            (f1, f2) match {
              case (FALSE, FALSE) => { FALSE }
              case (TRUE, b) => { TRUE }
              case (a, TRUE) => { TRUE }
              case (a, b) => { preEval(ORELSE(preEval(a), preEval(b))) }
            }
          }
          case IMPLY(f1, f2) => {
            (f1, f2) match {
              case (TRUE, FALSE) => { FALSE }
              case (TRUE, TRUE) => { TRUE }
              case (FALSE, b) => { TRUE }
              case (a, b) => { preEval(IMPLY(preEval(a), preEval(b))) }
            }
          }
          case LESS(e1, e2) => {
            (e1, e2) match {
              case (NUM(a), NUM(b)) => { if (a < b) TRUE else FALSE }
              case (a, b) => { preEval(LESS(exprSol(a), exprSol(b))) }
            }
          }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        form match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(f) => {
            f match {
              case TRUE => { false }
              case FALSE => { true }
              case a => { eval(NOT(preEval(f))) }
            }
          }
          case ANDALSO(f1, f2) => {
            (f1, f2) match {
              case (TRUE, TRUE) => { true }
              case (FALSE, b) => { false }
              case (a, FALSE) => { false }
              case (a, b) => { eval(ANDALSO(preEval(a), preEval(b))) }
            }
          }
          case ORELSE(f1, f2) => {
            (f1, f2) match {
              case (FALSE, FALSE) => { false }
              case (TRUE, b) => { true }
              case (a, TRUE) => { true }
              case (a, b) => { eval(ORELSE(preEval(a), preEval(b))) }
            }
          }
          case IMPLY(f1, f2) => {
            (f1, f2) match {
              case (TRUE, FALSE) => { false }
              case (TRUE, TRUE) => { true }
              case (FALSE, b) => { true }
              case (a, b) => { eval(IMPLY(preEval(a), preEval(b))) }
            }
          }
          case LESS(e1, e2) => {
            (e1, e2) match {
              case (NUM(a), NUM(b)) => { if (a < b) true else false }
              case (a, b) => { eval(LESS(exprSol(a), exprSol(b))) }
            }
          }
        }
    }
  )
  		
}