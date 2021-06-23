import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub461 {
  /*Seok Jin Lee 2013-11417 CSE*/
  
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
  
  /*
  let rec evalexpr(exp: expr): int = 
  	match exp with
  	| NUM n -> n
  	| PLUS(l,r) -> evalexpr(l) + evalexpr(r)
  	| MINUS(l,r) -> evalexpr(l) - evalexpr(r)
  */
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      def evalexpr(exp: Expr): Int63 = {
        exp match {
          case NUM(n) => { n }
          case PLUS(l, r) => { evalexpr(l) + evalexpr(r) }
          case MINUS(l, r) => { evalexpr(l) - evalexpr(r) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(op) => { not(eval(op)) }
        case ANDALSO(l, r) => { eval(l) && eval(r) }
        case ORELSE(l, r) => { eval(l) || eval(r) }
        case IMPLY(l, r) => { not(eval(l)) || eval(r) }
        case LESS(l, r) => { evalexpr(l) < evalexpr(r) }
      }
    }
  } 
  
}
