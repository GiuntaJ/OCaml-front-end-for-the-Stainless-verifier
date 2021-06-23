import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub181 {
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
          def compute(exp) = {
            exp match {
              case Num(n) => { n }
              case Plus(exp1, exp2) => { compute(exp1) + compute(exp2) }
              case Minus(exp1, exp2) => { compute(exp1) - compute(exp2) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(f) => { not(eval(f)) }
            case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
            case OrElse(f1, f2) => { eval(f1) || eval(f2) }
            case Equal(exp1, exp2) => { compute(exp1) eq compute(exp2) }
            case Imply(f1, f2) => {
              (eval(f1), eval(f2)) match {
                case (true, false) => { false }
                case (_, _) => { true }
              }
            }
          }
        }
    }
  )
            
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}