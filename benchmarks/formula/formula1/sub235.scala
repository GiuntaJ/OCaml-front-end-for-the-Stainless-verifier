import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub235 {
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
  
  def eval(fm: Formula): Boolean = {
    val _2 = {
      def imp(a, b) = { if (a == true && b == false) false else true }
      val _3 = {
        def exp(ex) = {
          ex match {
            case NUM(a) => { a }
            case PLUS(a, b) => { exp(a) + exp(b) }
            case MINUS(a, b) => { exp(a) - exp(b) }
          }
        }
        fm match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(a) => { not(eval(a)) }
          case ANDALSO(a, b) => { eval(a) && eval(b) }
          case ORELSE(a, b) => { eval(a) || eval(b) }
          case IMPLY(a, b) => { imp(eval(a), eval(b)) }
          case LESS(a, b) => { exp(a) < exp(b) }
        }
      }
    }
  }
}