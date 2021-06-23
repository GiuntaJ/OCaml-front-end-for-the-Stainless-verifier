import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub231 {
  /*
  ** PL::HW[01].Problem[02]
  ** 
  ** Last Mod.: 2014-09-14 20:34
  ** Writ. by : CMS
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
  
  def calc(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(le, re) => { calc(le) + calc(re) }
      case MINUS(le, re) => { calc(le) - calc(re) }
    }
  }
  
  def eval(e: Formula): Boolean = {
    e match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(f) => { if (eval(f) == true) false else true }
      case ANDALSO(l, r) => {
        if (eval(l) == true) if (eval(r) == true) true else false else false
      }
      case ORELSE(l, r) => {
        if (eval(l) == false) if (eval(r) == false) false else true else true
      }
      case IMPLY(l, r) => {
        
          if (
            eval(l) == false
          ) {
            true 
          } else if (
            eval(r) == true
          ) {
            true 
          } else {
            false
          }
      }
      case LESS(le, re) => { if (calc(le) < calc(re)) true else false }
    }
  }
}
