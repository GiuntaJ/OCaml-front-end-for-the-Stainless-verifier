import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub194 {
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
          def exp2int(g) = {
            g match {
              case Num(a) => { a }
              case Plus(a, b) => { exp2int(a) + exp2int(b) }
              case Minus(a, b) => { exp2int(a) - exp2int(b) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(p) => { not(eval(p)) }
            case AndAlso(p, q) => { eval(p) && eval(q) }
            case OrElse(p, q) => { eval(p) || eval(q) }
            case Imply(p, q) => { not(eval(p)) || eval(q) }
            case Equal(p, q) => { exp2int(p) == exp2int(q) }
          }
        }
    }
  )
      
}