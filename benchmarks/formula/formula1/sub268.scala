import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub268 {
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
          /* eval 을 rec 로 만들지 말고 이 안에 새로운 함수를 만들어 comp가 반복되지 않게 한다.*/
  
  def eval(arg: Formula): Boolean = {
    val _2 = {
      def comp(k) = {
        k match {
          case NUM(a) => { a }
          case PLUS(b, c) => { comp(b) + comp(c) }
          case MINUS(d, e) => { comp(d) - comp(e) }
        }
      }
      arg match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(v1) => { not(eval(v1)) }
        case ANDALSO(v2, v3) => { eval(v2) && eval(v3) }
        case ORELSE(v4, v5) => { eval(v4) || eval(v5) }
        case IMPLY(v6, v7) => { if (eval(v6) == false) true else eval(v7) }
        case LESS(v8, v9) => { if (comp(v8) < comp(v9)) true else false }
      }
    }
  }
}
