import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub415 {
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
  
  def count(a: Expr): Int63 = {
    a match {
      case NUM(n) => { n }
      case PLUS(m, n) => { count(m) + count(n) }
      case MINUS(m, n) => { count(m) - count(n) }
    }
  }
  
  def eval(a: Formula): Boolean = {
    a match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(nota) => { not(eval(nota)) }
      case ANDALSO(aa, ab) => { eval(aa) && eval(ab) }
      case ORELSE(aa, ab) => { eval(aa) || eval(ab) }
      case IMPLY(aa, ab) => { if (eval(aa) && not(eval(ab))) false else true }
      case LESS(expm, expn) => { if (count(expm) < count(expn)) true else false
      }
    }
  }
}
