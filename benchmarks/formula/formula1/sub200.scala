import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub200 {
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
  
  def getexpr(inpt: Expr): Int63 = {
    inpt match {
      case NUM(form) => { form }
      case PLUS(form, lat) => { getexpr(form) + getexpr(lat) }
      case MINUS(form, lat) => { getexpr(form) - getexpr(lat) }
    }
  }
  
  def eval(input: Formula): Boolean = {
    input match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(inpt) => { eval(inpt) }
      case ANDALSO(form, lat) => { eval(form) && eval(lat) }
      case ORELSE(form, lat) => { eval(form) || eval(lat) }
      case IMPLY(form, lat) => { not(eval(form)) || eval(lat) }
      case LESS(form, lat) => { getexpr(form) < getexpr(lat) }
    }
  }
}