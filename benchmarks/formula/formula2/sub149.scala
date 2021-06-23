import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub149 {
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
          def evalexp: Exp => Int63 = (
            (e) =>
              {
                e match {
                  case Num(a) => { a }
                  case Plus(a, b) => { evalexp(a) + evalexp(b) }
                  case Minus(a, b) => { evalexp(a) - evalexp(b) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(a) => { not(eval(a)) }
            case AndAlso(a, b) => { eval(a) && eval(b) }
            case OrElse(a, b) => { eval(a) || eval(b) }
            case Imply(a, b) => { not(eval(a)) || eval(b) }
            case Equal(a, b) => { if (evalexp(a) == evalexp(b)) true else false
            }
          }
        }
    }
  )
}