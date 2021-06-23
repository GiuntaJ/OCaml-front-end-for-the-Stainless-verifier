import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub493 {
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
  
  val eval: Formula => Boolean = (
    (x) =>
      {
        x match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(y) => {
            y match {
              case TRUE => { false }
              case FALSE => { true }
            }
          }
          case ANDALSO(x, y) => {
            (x, y) match {
              case (TRUE, TRUE) => { true }
              case (FALSE, _) => { false }
              case (_, FALSE) => { false }
            }
          }
          case ORELSE(x, y) => {
            (x, y) match {
              case (TRUE, _) => { true }
              case (_, TRUE) => { true }
              case (FALSE, FALSE) => { false }
            }
          }
          case IMPLY(x, y) => {
            (x, y) match {
              case (FALSE, _) => { true }
              case (TRUE, TRUE) => { true }
              case (TRUE, FALSE) => { false }
            }
          }
          case LESS(a, b) => {
            (a, b) match {
              case (PLUS(NUM(a), NUM(b)), PLUS(NUM(c), NUM(d))) => {
                if (a + b >= c + d) false else true
              }
              case (PLUS(NUM(a), NUM(b)), MINUS(NUM(c), NUM(d))) => {
                if (a + b >= c - d) false else true
              }
              case (MINUS(NUM(a), NUM(b)), PLUS(NUM(c), NUM(d))) => {
                if (a - b >= c + d) false else true
              }
              case (MINUS(NUM(a), NUM(b)), MINUS(NUM(c), NUM(d))) => {
                if (a - b >= c - d) false else true
              }
              case (NUM(a), NUM(b)) => { if (a >= b) false else true }
            }
          }
        }
    }
  )
}