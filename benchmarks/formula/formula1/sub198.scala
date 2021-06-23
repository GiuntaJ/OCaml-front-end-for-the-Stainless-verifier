import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub198 {
  /*
  	department : computer science & engineering
  	student ID : 2012-11242 / name : Seon-bi, Park
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
  
  
  def cal(num: Expr): Int63 = {
    num match {
      case NUM(lnum) => { lnum }
      case PLUS(lnum, rnum) => { cal(lnum) + cal(rnum) }
      case MINUS(lnum, rnum) => { cal(lnum) - cal(rnum) }
    }
  }
  
  
  def eval(logic: Formula): Boolean = {
    logic match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fst) => { not(eval(fst)) }
      case ANDALSO(fst, snd) => { eval(fst) && eval(snd) }
      case ORELSE(fst, snd) => { eval(fst) || eval(snd) }
      case IMPLY(fst, snd) => {
        
          if (
            eval(fst) == false
          ) {
            true 
          } else if (
            eval(snd) == true
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(fst, snd) => { if (cal(fst) < cal(snd)) true else false }
    }
  }
}