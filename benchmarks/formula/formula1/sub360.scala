import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub360 {
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
    (a) =>
      {
        a match {
          case TRUE => { true }
          case FALSE => { false }
          case NOT(TRUE) => { false }
          case NOT(FALSE) => { true }
          case ANDALSO(TRUE, TRUE) => { true }
          case ANDALSO(TRUE, FALSE) => { false }
          case ANDALSO(FALSE, TRUE) => { false }
          case ANDALSO(FALSE, FALSE) => { false }
          case ORELSE(TRUE, TRUE) => { true }
          case ORELSE(TRUE, FALSE) => { true }
          case ORELSE(FALSE, TRUE) => { true }
          case ORELSE(FALSE, FALSE) => { false }
          case IMPLY(TRUE, FALSE) => { false }
          case IMPLY(TRUE, TRUE) => { true }
          case IMPLY(FALSE, TRUE) => { true }
          case IMPLY(FALSE, FALSE) => { true }
          case LESS(NUM(b), NUM(c)) => { if (b > c) false else true }
          case LESS(PLUS(NUM(b), NUM(c)), PLUS(NUM(d), NUM(e))) => {
            val _11 = {
              val x = b + c
              val y = d + e
              if (x > y) false else true
            }
          }
          case LESS(PLUS(NUM(b), NUM(c)), MINUS(NUM(d), NUM(e))) => {
            val _8 = {
              val x = b + c
              val y = d - e
              if (x > y) false else true
            }
          }
          case LESS(MINUS(NUM(b), NUM(c)), PLUS(NUM(d), NUM(e))) => {
            val _5 = {
              val x = b - c
              val y = d + e
              if (x > y) false else true
            }
          }
          case LESS(MINUS(NUM(b), NUM(c)), MINUS(NUM(d), NUM(e))) => {
            val _2 = {
              val x = b - c
              val y = d - e
              if (x > y) false else true
            }
          }
        }
    }
  )
}
