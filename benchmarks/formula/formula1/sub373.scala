import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula1_sub373 {
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
  
  def expToint(exp1: Expr): Int63 = {
    exp1 match {
      case NUM(in1) => { in1 }
      case PLUS(in2, in3) => { expToint(in2) + expToint(in3) }
      case MINUS(in4, in5) => { expToint(in4) - expToint(in5) }
    }
  }
  
  
  
  def eval(form1: Formula): Boolean = {
    form1 match {
      case TRUE => { true }
      case FALSE => { false }
      case NOT(fo1) => { not(eval(fo1)) }
      case ANDALSO(fo2, fo3) => { eval(fo2) && eval(fo3) }
      case ORELSE(fo4, fo5) => { eval(fo4) || eval(fo5) }
      case IMPLY(fo6, fo7) => { not(eval(fo6) && not(eval(fo7))) }
      case LESS(ex1, ex2) => { expToint(ex1) < expToint(ex2) }
    }
  }
}