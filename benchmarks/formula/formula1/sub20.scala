import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub20 {
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
      def evalExpr(ep) = {
        ep match {
          case NUM(n) => { n }
          case PLUS(a, b) => { evalExpr(a) + evalExpr(b) }
          case MINUS(a, b) => { evalExpr(a) - evalExpr(b) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(a) => { not(eval(a)) }
        case ANDALSO(a, b) => { if (eval(a)) eval(b) else false }
        case ORELSE(a, b) => { if (eval(a)) true else eval(b) }
        case IMPLY(a, b) => { if (eval(a)) eval(b) else true }
        case LESS(a, b) => { evalExpr(a) < evalExpr(b) }
      }
    }
  }
  /*
  let check f =
  	if( eval f ) then print_string "true
  "
  	else print_string "false
  ";;
  
  
  check( IMPLY( LESS( PLUS( NUM 10, NUM 15 ), NUM 25 ), LESS( PLUS( NUM 10, NUM 15 ), NUM 25 ) ) );;*/
}