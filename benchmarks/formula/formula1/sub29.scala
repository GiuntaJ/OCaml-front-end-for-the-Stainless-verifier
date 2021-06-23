import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub29 {
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
      def solve(e) = {
        e match {
          case NUM(a) => { a }
          case PLUS(e1, e2) => { solve(e1) + solve(e2) }
          case MINUS(e1, e2) => { solve(e1) - solve(e2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(f2) => { not(eval(f2)) }
        case ANDALSO(f2, f3) => { eval(f2) && eval(f3) }
        case ORELSE(f2, f3) => { eval(f2) || eval(f3) }
        case IMPLY(f2, f3) => { not(eval(f2)) || eval(f3) }
        case LESS(e1, e2) => { solve(e1) < solve(e2) }
      }
    }
  }
  
  /*
  let testform = IMPLY (LESS (NUM 45, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT TRUE));;
  let testform2 = IMPLY (LESS (NUM 45, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT FALSE));;
  let testform3 = IMPLY (LESS (NUM 145, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT TRUE));;
  let testform4 = IMPLY (LESS (NUM 145, PLUS (NUM 23, MINUS (NUM 65, NUM 32))), ANDALSO (ORELSE (TRUE, FALSE),NOT FALSE));;
  */
}