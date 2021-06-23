import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub59 {
  sealed abstract class Formula {}
  case object True_ extends Formula {}
  case object False_ extends Formula {}
  case class Not(param0: Formula) extends Formula {}
  case class AndAlso(param0: Formula,  param1: Formula) extends Formula {}
  case class OrElse(param0: Formula,  param1: Formula) extends Formula {}
  case class Imply(param0: Formula,  param1: Formula) extends Formula {}
  case class Equal(param0: Exp,  param1: Exp) extends Formula {}
  
  sealed abstract class Exp {}
  case class Num(param0: Int63) extends Exp {}
  case class Plus(param0: Exp,  param1: Exp) extends Exp {}
  case class Minus(param0: Exp,  param1: Exp) extends Exp {} 
  
  
  
  def eval_op(op: Exp): Int63 = {
    op match {
      case Num(a) => { a }
      case Minus(exp1, exp2) => { eval_op(exp1) - eval_op(exp2) }
      case Plus(exp1, exp2) => { eval_op(exp1) + eval_op(exp2) }
    }
  }
  def eval(f) = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(f) => { not(eval(f)) }
      case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
      case OrElse(f1, f2) => { eval(f1) || eval(f2) }
      case Imply(f1, f2) => { not(eval(f1)) && eval(f2) }
      case Equal(exp1, exp2) => {
        if (eval_op(exp1) == eval_op(exp2)) true else false
      }
    }
  }
}
