import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub25 {
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
          def ma2(e) = {
            e match {
              case Num(x) => { x }
              case Plus(x, y) => { ma2(x) + ma2(y) }
              case Minus(x, y) => { ma2(x) - ma2(y) }
            }
          }
          val _5 = {
            def ma(p) = {
              p match {
                case True_ => { true }
                case False_ => { false }
                case Not(x) => { not(ma(x)) }
                case AndAlso(x, y) => { ma(x) && ma(y) }
                case OrElse(x, y) => { ma(x) || ma(y) }
                case Imply(x, y) => {
                  if (ma(x) == true && ma(y) == false) false else true
                }
                case Equal(x, y) => { if (ma2(x) == ma2(y)) true else false }
              }
            }
            ma(f)
          }
        }
    }
  )
}