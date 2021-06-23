import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub15 {
  /* School of Computer Science & Engineering
   * 2009-23151
   * Sungkeun Cho
   * HW 2 - Exercise 3
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
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      def eval_e(e) = {
        e match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { eval_e(e1) + eval_e(e2) }
          case MINUS(e1, e2) => { eval_e(e1) - eval_e(e2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f1) => { not(eval(f1)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
        case LESS(e1, e2) => { eval_e(e1) < eval_e(e2) }
      }
    }
  }
  
        
  /*
  eval(TRUE);;
  eval(IMPLY(TRUE,ANDALSO(NOT(FALSE),TRUE)));;
  eval(IMPLY(LESS(NUM(2),NUM(3))
  	     ,ANDALSO(NOT(FALSE),TRUE)));;
  eval(IMPLY(LESS(PLUS(NUM(1),NUM(2))
  		  ,(MINUS(NUM(5),NUM(1))))
  	     ,ANDALSO(NOT(FALSE),TRUE)));;
  */
}