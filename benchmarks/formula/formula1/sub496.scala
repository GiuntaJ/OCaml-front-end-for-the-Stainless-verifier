import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub496 {
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
  
  
  def eval(fn: Formula): Boolean = {
    fn match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(something) => {
        something match {
          case TRUE => { false }
          case FALSE => { true }
        }
      }
      case ANDALSO(TRUE, TRUE) => { true }
      case ANDALSO(TRUE, FALSE) => { false }
      case ANDALSO(FALSE, _) => { false }
      case ORELSE(FALSE, FALSE) => { false }
      case ORELSE(FALSE, TRUE) => { true }
      case ORELSE(TRUE, _) => { true }
      case IMPLY(TRUE, FALSE) => { false }
      case IMPLY(TRUE, TRUE) => { true }
      case IMPLY(FALSE, _) => { true }
      case LESS(NUM(n1), NUM(n2)) => { n1 < n2 }
      case LESS(PLUS(NUM(n1), NUM(n2)), PLUS(NUM(n3), NUM(n4))) => {
        n1 + n2 < n3 + n4
      }
      case LESS(PLUS(NUM(n1), NUM(n2)), MINUS(NUM(n3), NUM(n4))) => {
        n1 + n2 < n3 - n4
      }
      case LESS(MINUS(NUM(n1), NUM(n2)), PLUS(NUM(n3), NUM(n4))) => {
        n1 - n2 < n3 + n4
      }
      case LESS(MINUS(NUM(n1), NUM(n2)), MINUS(NUM(n3), NUM(n4))) => {
        n1 - n2 < n3 - n4
      }
    }
  }
}