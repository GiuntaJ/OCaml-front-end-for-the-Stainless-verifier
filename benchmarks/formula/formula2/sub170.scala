import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub170 {
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
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def calc: Exp => Int63 = (
            (expr) =>
              {
                expr match {
                  case Num(x) => { x }
                  case Plus(x, y) => { calc(x) + calc(y) }
                  case Minus(x, y) => { calc(x) - calc(y) }
                }
            }
          )
          val _5 = {
            def evaluate: Formula => Boolean = (
              (form) =>
                {
                  form match {
                    case True_ => { true }
                    case False_ => { false }
                    case Equal(x, y) => {
                      if (calc(x) == calc(y)) true else false
                    }
                    case Not(x) => { not(evaluate(x)) }
                    case AndAlso(x, y) => { evaluate(x) && evaluate(y) }
                    case OrElse(x, y) => { evaluate(x) || evaluate(y) }
                    case Imply(x, y) => { not(evaluate(x)) || evaluate(y) }
                  }
              }
            )
            evaluate(f)
          }
        }
    }
  )
}
