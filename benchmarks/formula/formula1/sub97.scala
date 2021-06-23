import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub97 {
  /* hw 1_5. */
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
      def calcu(e) = {
        e match {
          case NUM(n) => { n }
          case PLUS(n1, n2) => { calcu(n1) + calcu(n2) }
          case MINUS(n1, n2) => { calcu(n1) - calcu(n2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { if (eval(a) == true) false else true }
        case ANDALSO(a, b) => {
          (eval(a), eval(b)) match {
            case (true, true) => { true }
            case _ => { false }
          }
        }
        case ORELSE(a, b) => {
          (eval(a), eval(b)) match {
            case (false, false) => { false }
            case _ => { true }
          }
        }
        case IMPLY(a, b) => {
          (eval(a), eval(b)) match {
            case (true, false) => { false }
            case _ => { true }
          }
        }
        case LESS(a, b) => { if (calcu(a) < calcu(b)) true else false }
      }
    }
  }
}