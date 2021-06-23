import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub20 {
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
          def expEval: Exp => Int63 = (
            (e) =>
              {
                e match {
                  case Num(x) => { x }
                  case Plus(a, b) => { expEval(a) + expEval(b) }
                  case Minus(a, b) => { expEval(a) - expEval(b) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(x) => { not(eval(x)) }
            case AndAlso(a, b) => { eval(a) && eval(b) }
            case OrElse(a, b) => { eval(a) || eval(b) }
            case Imply(a, b) => { not(eval(a)) || eval(b) }
            case Equal(a, b) => { if (expEval(a) == expEval(b)) true else false
            }
          }
        }
    }
  )
}
