import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub108 {
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
  
  
  def eval: Formula => Boolean = (
    (form) =>
      {
        val _4 = {
          def value: Exp => Int63 = (
            (x) =>
              {
                x match {
                  case Num(a) => { a }
                  case Plus(a, b) => { value(a) + value(b) }
                  case Minus(a, b) => { value(a) * value(b) }
                }
            }
          )
          form match {
            case True_ => { true }
            case False_ => { false }
            case Not(x) => { not(eval(x)) }
            case AndAlso(x, y) => { eval(x) && eval(y) }
            case OrElse(x, y) => { eval(x) || eval(y) }
            case Imply(x, y) => { not(eval(x)) || eval(y) }
            case Equal(x, y) => { value(x) == value(y) }
          }
        }
    }
  )
}
