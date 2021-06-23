import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub79 {
  sealed abstract class Exp {}
  case class Num(param0: Int63) extends Exp {}
  case class Plus(param0: Exp,  param1: Exp) extends Exp {}
  case class Minus(param0: Exp,  param1: Exp) extends Exp {} 
  
  sealed abstract class Formula {}
  case object True_ extends Formula {}
  case object False_ extends Formula {}
  case class Not(param0: Formula) extends Formula {}
  case class AndAlso(param0: Formula,  param1: Formula) extends Formula {}
  case class OrElse(param0: Formula,  param1: Formula) extends Formula {}
  case class Imply(param0: Formula,  param1: Formula) extends Formula {}
  case class Equal(param0: Exp,  param1: Exp) extends Formula {}
  
  def eval_exp(a: Exp): Int63 = {
    a match {
      case Num(n) => { n }
      case Plus(n1, n2) => { eval_exp(n1) + eval_exp(n2) }
      case Minus(n1, n2) => { eval_exp(n1) + eval_exp(n2) }
    }
  }
  
  
  def eval(thing: Formula): Boolean = {
    thing match {
      case True_ => { true }
      case False_ => { false }
      case Not(a) => { if (eval(a)) false else true }
      case AndAlso(a, b) => { eval(a) && eval(b) }
      case OrElse(a, b) => { eval(a) || eval(b) }
      case Imply(a, b) => {
        if (eval(a) == true && eval(b) == false) false else true
      }
      case Equal(a, b) => { if (eval_exp(a) == eval_exp(b)) true else false }
    }
  }
}