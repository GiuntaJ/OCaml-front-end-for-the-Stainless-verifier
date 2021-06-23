import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub408 {
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
  
  def expr_eval(e: Expr): Int63 = {
    e match {
      case NUM(i) => { i }
      case PLUS(fst, snd) => { expr_eval(fst) + expr_eval(snd) }
      case MINUS(fst, snd) => { expr_eval(fst) - expr_eval(snd) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fm) => { not(eval(fm)) }
      case ANDALSO(fst, snd) => { eval(fst) && eval(snd) }
      case ORELSE(fst, snd) => { eval(fst) || eval(snd) }
      case IMPLY(fst, snd) => { not(eval(fst)) || eval(snd) }
      case LESS(fst, snd) => { expr_eval(fst) < expr_eval(snd) }
    }
  }
}