import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub17 {
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
  
  def equal_help(exp: Exp): Int63 = {
    exp match {
      case Num(n) => { n }
      case Plus(exp1, exp2) => { equal_help(exp1) + equal_help(exp2) }
      case Minus(exp1, exp2) => { equal_help(exp1) - equal_help(exp2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(f1) => { if (eval(f1) eq true) false else true }
      case AndAlso(f1, f2) => {
        if (eval(f1) && eval(f2) eq true) true else false
      }
      case OrElse(f1, f2) => { if (eval(f1) || eval(f2) eq true) true else false
      }
      case Imply(f1, f2) => {
        if (eval(f1) eq true && eval(f2) eq false) false else true
      }
      case Equal(exp1, exp2) => {
        if (equal_help(exp1) eq equal_help(exp2)) true else false
      }
    }
  }
}