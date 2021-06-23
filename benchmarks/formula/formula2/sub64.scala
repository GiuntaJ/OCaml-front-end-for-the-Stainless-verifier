import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub64 {
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
  
  def cal(e: Exp): Int63 = {
    e match {
      case Num(e1) => { e1 }
      case Plus(e1, e2) => { cal(e1) + cal(e2) }
      case Minus(e1, e2) => { cal(e1) - cal(e2) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(f1) => { if (eval(f1) eq true) false else true }
      case AndAlso(f1, f2) => {
        if (eval(f1) eq true && eval(f2) eq true) true else false
      }
      case OrElse(f1, f2) => {
        if (eval(f1) eq false && eval(f2) eq false) false else true
      }
      case Imply(f1, f2) => {
        if (eval(f1) eq true && eval(f2) eq false) false else true
      }
      case Equal(e1, e2) => { if (cal(e1) eq cal(e2)) true else false }
    }
  }
}