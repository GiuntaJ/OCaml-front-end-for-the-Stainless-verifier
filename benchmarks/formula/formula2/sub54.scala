import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub54 {
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
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def expfun(f) = {
            f match {
              case Num(a) => { a }
              case Plus(a, b) => { expfun(a) + expfun(b) }
              case Minus(a, b) => { expfun(a) - expfun(b) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(a) => { if (eval(a) == true) false else true }
            case AndAlso(a, b) => {
              if (eval(a) == true && eval(b) == true) true else false
            }
            case OrElse(a, b) => {
              if (eval(a) == false && eval(b) == false) false else true
            }
            case Imply(a, b) => {
              
                if (
                  eval(a) == false
                ) {
                  true 
                } else if (
                  eval(b) == true
                ) {
                  true 
                } else {
                  false
                }
            }
            case Equal(a, b) => { if (expfun(a) == expfun(b)) true else false }
          }
        }
    }
  )
}