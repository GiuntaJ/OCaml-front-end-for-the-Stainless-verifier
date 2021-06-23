import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub185 {
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
          def eval_exp: Exp => Int63 = (
            (num) =>
              {
                num match {
                  case Num(n) => { n }
                  case Plus(n1, n2) => { eval_exp(n1) + eval_exp(n2) }
                  case Minus(n1, n2) => { eval_exp(n1) - eval_exp(n2) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(p) => { not(eval(p)) }
            case AndAlso(p, q) => { eval(p) && eval(q) }
            case OrElse(p, q) => { eval(p) || eval(q) }
            case Imply(p, q) => { not(eval(p)) || eval(q) }
            case Equal(n1, n2) => { eval_exp(n1) == eval_exp(n2) }
          }
        }
    }
  )
}