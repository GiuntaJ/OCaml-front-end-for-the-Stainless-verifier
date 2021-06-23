import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub150 {
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
          def exp(f) = {
            f match {
              case Num(x) => { x }
              case Plus(x, y) => { exp(x) + exp(y) }
              case Minus(x, y) => { exp(x) - exp(y) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(x) => { if (eval(x) == true) false else true }
            case AndAlso(x, y) => {
              if (eval(x) == true || eval(y) == true) true else false
            }
            case OrElse(x, y) => {
              if (eval(x) == false && eval(y) == false) false else true
            }
            case Imply(x, y) => {
              if (eval(x) == true && eval(y) == false) false else true
            }
            case Equal(x, y) => { if (exp(x) == exp(y)) true else false }
          }
        }
    }
  )
}