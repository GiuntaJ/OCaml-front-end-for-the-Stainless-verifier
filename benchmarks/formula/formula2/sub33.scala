import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub33 {
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
  
  def eval: Formula => Boolean = val _0 = {
    def expfunc(e) = {
      e match {
        case Num(k) => { k }
        case Plus(k, l) => { expfunc(k) + expfunc(l) }
        case Minus(k, l) => { expfunc(k) - expfunc(l) }
      }
    }
    (
      (f) =>
        {
          f match {
            case Not(k) => { if (eval(k) eq true) false else true }
            case AndAlso(k, l) => {
              if (eval(k) eq true && eval(l) eq true) true else false
            }
            case OrElse(k, l) => {
              if (eval(k) eq true || eval(l) eq true) true else false
            }
            case Imply(k, l) => {
              if (eval(k) eq true && eval(l) eq false) false else true
            }
            case Equal(k, l) => { if (expfunc(k) eq expfunc(l)) true else false
            }
            case True_ => { true }
            case False_ => { false }
          }
      }
    )
  }
}