import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub90 {
  /* 200511843 LEE JONGHO */
  
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
  
  
  def imply(((f1, f2))) = {
    if (f1 == true) if (f2 == true) true else false else true
  }
  
  def orelse(((f1, f2))) = { if (f1 == true || f2 == true) true else false }
  
  def andalso(((f1, f2))) = { if (f1 == true && f2 == true) true else false }
  
  
  def less(((e1, e2))) = {
    val _2 = {
      def cal(ex) = {
        ex match {
          case NUM(x) => { x }
          case PLUS(e1, e2) => { cal(e1) + cal(e2) }
          case MINUS(e1, e2) => { cal(e1) - cal(e2) }
        }
      }
      if (cal(e1) < cal(e2)) true else false
    }
  }
  
   
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fom) => { not(eval(fom)) }
      case IMPLY(f1, f2) => { imply(eval(f1), eval(f2)) }
      case LESS(e1, e2) => { less(e1, e2) }
      case ORELSE(f1, f2) => { orelse(eval(f1), eval(f2)) }
      case ANDALSO(f1, f2) => { andalso(eval(f1), eval(f2)) }
    }
  }
}