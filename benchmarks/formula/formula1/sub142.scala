import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub142 {
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
      def eval_num(e) = {
        e match {
          case NUM(e) => { e }
          case PLUS(e1, e2) => { eval_num(e1) + eval_num(e2) }
          case MINUS(e1, e2) => { eval_num(e1) - eval_num(e2) }
        }
      }
      f match {
        case TRUE => { true }
        case FALSE => { false }
        case NOT(fp) => { not(eval(fp)) }
        case ANDALSO(fp1, fp2) => { eval(fp1) && eval(fp2) }
        case ORELSE(fp1, fp2) => { eval(fp1) || eval(fp2) }
        case IMPLY(fp1, fp2) => { not(eval(fp1)) || eval(fp2) }
        case LESS(e1, e2) => { eval_num(e1) < eval_num(e2) }
      }
    }
  }
}