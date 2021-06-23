import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub29 {
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
  
  def exp_eval(e: Exp): Int63 = {
    e match {
      case Num(a) => { a }
      case Plus(a, b) => { exp_eval(a) + exp_eval(b) }
      case Minus(a, b) => { exp_eval(a) - exp_eval(b) }
    }
  }
  
  def eval(s: Formula): Boolean = {
    s match {
      case True_ => { true }
      case False_ => { false }
      case Not(f) => { if (eval(f)) false else true }
      case AndAlso(a, b) => { if (eval(a) && eval(b)) true else false }
      case OrElse(a, b) => { if (eval(a) || eval(b)) true else false }
      case Imply(a, b) => { if (eval(a) && eval(b) eq false) false else true }
      case Equal(a, b) => { if (exp_eval(a) eq exp_eval(b)) true else false }
    }
  }
         
}