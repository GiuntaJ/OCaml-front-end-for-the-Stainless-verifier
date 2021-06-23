import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub371 {
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
      case NOT(subf) => { not(eval(subf)) }
      case ANDALSO(sf1, sf2) => { eval(sf1) && eval(sf2) }
      case ORELSE(sf1, sf2) => { eval(sf1) || eval(sf2) }
      case IMPLY(sf1, sf2) => { not(eval(sf1)) || eval(sf2) }
      case LESS(e1, e2) => {
        val _2 = {
          def evalexpr(e: Expr): Int63 = {
            e match {
              case NUM(i) => { i }
              case PLUS(se1, se2) => { evalexpr(se1) + evalexpr(se2) }
              case MINUS(se1, se2) => { evalexpr(se1) - evalexpr(se2) }
            }
          }
          evalexpr(e1) < evalexpr(e2)
        }
      }
    }
  }
}