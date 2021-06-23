import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub305 {
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
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(form) => { not(eval(form)) }
      case ANDALSO(form, latt) => { eval(form) && eval(latt) }
      case ORELSE(form, latt) => { eval(form) || eval(latt) }
      case IMPLY(form, latt) => { not(eval(form)) || eval(latt) }
      case LESS(form, latt) => {
        if (getval(form) < getval(latt)) true else false
      }
    }
  }
  def getval(s) = {
    s match {
      case NUM(n) => { n }
      case PLUS(n, m) => { getval(n) + getval(m) }
      case MINUS(n, m) => { getval(n) - getval(m) }
    }
  }
}