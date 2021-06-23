import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub466 {
  /*
  	CSE / 2013-11426 / Im DongYeop
  	Homework 2: Exercise 1
  */
  
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
  
  def gonum(e: Expr): Expr = {
    e match {
      case NUM(ein) => { NUM(ein) }
      case PLUS(ein1, ein2) => {
        (ein1, ein2) match {
          case (NUM(eein1), NUM(eein2)) => { NUM(eein1 + eein2) }
          case _ => { gonum(PLUS(gonum(ein1), gonum(ein2))) }
        }
      }
      case MINUS(ein1, ein2) => {
        (ein1, ein2) match {
          case (NUM(eein1), NUM(eein2)) => { NUM(eein1 - eein2) }
          case _ => { gonum(MINUS(gonum(ein1), gonum(ein2))) }
        }
      }
    }
  }
  
  def goform(f: Formula): Formula = {
    f match {
      case TRUE => { TRUE }
      case FALSE => { FALSE }
      case NOT(ff) => {
        ff match {
          case TRUE => { FALSE }
          case FALSE => { TRUE }
          case _ => { NOT(goform(ff)) }
        }
      }
      case ANDALSO(f1, f2) => {
        (f1, f2) match {
          case (TRUE, TRUE) => { TRUE }
          case (FALSE, _) => { FALSE }
          case (_, FALSE) => { FALSE }
          case _ => { ANDALSO(goform(f1), goform(f2)) }
        }
      }
      case ORELSE(f1, f2) => {
        (f1, f2) match {
          case (FALSE, FALSE) => { FALSE }
          case (TRUE, _) => { TRUE }
          case (_, TRUE) => { TRUE }
          case _ => { ORELSE(goform(f1), goform(f2)) }
        }
      }
      case IMPLY(f1, f2) => {
        (f1, f2) match {
          case (TRUE, FALSE) => { FALSE }
          case (_, TRUE) => { TRUE }
          case (FALSE, FALSE) => { TRUE }
          case _ => { IMPLY(goform(f1), goform(f2)) }
        }
      }
      case LESS(e1, e2) => {
        (e1, e2) match {
          case (NUM(ein1), NUM(ein2)) => { if (ein1 < ein2) TRUE else FALSE }
          case _ => { LESS(gonum(e1), gonum(e2)) }
        }
      }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(ff) => { eval(goform(NOT(ff))) }
      case ANDALSO(f1, f2) => { eval(goform(ANDALSO(f1, f2))) }
      case ORELSE(f1, f2) => { eval(goform(ORELSE(f1, f2))) }
      case IMPLY(f1, f2) => { eval(goform(IMPLY(f1, f2))) }
      case LESS(e1, e2) => {
        (e1, e2) match {
          case (NUM(ein1), NUM(ein2)) => { ein1 < ein2 }
          case _ => { eval(LESS(gonum(e1), gonum(e2))) }
        }
      }
    }
  }
}
