import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub186 {
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
          def expEval(exp) = {
            exp match {
              case Num(i) => { i }
              case Plus(exp1, exp2) => { expEval(exp1) + expEval(exp2) }
              case Minus(exp1, exp2) => { expEval(exp1) - expEval(exp2) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(g) => { not(eval(g)) }
            case AndAlso(g, h) => { eval(g) && eval(h) }
            case OrElse(g, h) => { eval(g) || eval(h) }
            case Imply(g, h) => { not(eval(g)) || eval(h) }
            case Equal(exp1, exp2) => { expEval(exp1) == expEval(exp2) }
          }
        }
    }
  )
}
