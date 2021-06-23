import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub116 {
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
      def cal_exp(exp) = {
        exp match {
          case NUM(i) => { i }
          case PLUS(e1, e2) => { cal_exp(e1) + cal_exp(e2) }
          case MINUS(e1, e2) => { cal_exp(e1) - cal_exp(e2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f_0) => { not(eval(f_0)) }
        case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
        case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
        case IMPLY(f1, f2) => {
          if (eval(f1) == true && eval(f2) == false) false else true
        }
        case LESS(e1, e2) => { if (cal_exp(e1) < cal_exp(e2)) true else false }
      }
    }
  }
  
    /*
  let e1 = PLUS(MINUS(PLUS(PLUS(NUM 1,NUM 2),NUM 3), NUM 4), NUM 5)/* 7*/
  let e2 = MINUS(PLUS(MINUS(PLUS(PLUS(NUM 1,NUM 2),NUM 3), NUM 4), NUM 5),NUM 2)
  /* 5*/
  
  let f1 = LESS(e1,e2) /* false */
  let f2 = NOT f1 /* true */
  let f3 = ANDALSO(f1, f2) /* false */
  let f4 = ORELSE(f2,f3) /* true */
  let f5 = IMPLY(f3,f4) /* true */
  */
}