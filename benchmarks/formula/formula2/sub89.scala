import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub89 {
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
          def calcExp(e) = {
            e match {
              case Num(a) => { a }
              case Plus(sub1, sub2) => { calcExp(sub1) + calcExp(sub2) }
              case Minus(sub1, sub2) => { calcExp(sub1) - calcExp(sub2) }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(sub_f) => { not(eval(sub_f)) }
            case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
            case OrElse(f1, f2) => { eval(f1) || eval(f2) }
            case Imply(f1, f2) => { not(eval(f1) && not(eval(f2))) }
            case Equal(e1, e2) => { calcExp(e1) == calcExp(e2) }
          }
        }
    }
  ) /* TODO */
}
