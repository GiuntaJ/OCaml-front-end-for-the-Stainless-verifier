import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub210 {
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
  def nexpr(e) = {
    e match {
      case NUM(n) => { n }
      case PLUS(e1, e2) => { nexpr(e1) + nexpr(e2) }
      case MINUS(e1, e2) => { nexpr(e1) - nexpr(e2) }
    }
  }
  def eval(f) = {
    f match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(ff) => { not(eval(ff)) }
      case ANDALSO(f1, f2) => { eval(f1) && eval(f2) }
      case ORELSE(f1, f2) => { eval(f1) || eval(f2) }
      case IMPLY(f1, f2) => { not(eval(f1)) || eval(f2) }
      case LESS(e1, e2) => { nexpr(e1) < nexpr(e2) }
    }
  }
}