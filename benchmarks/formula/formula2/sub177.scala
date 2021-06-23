import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub177 {
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
  
  def calculator: Exp => Int63 = (
    (x) =>
      {
        x match {
          case Num(a_0) => { a_0 }
          case Plus(a_0, b_0) => { calculator(a_0) + calculator(b_0) }
          case Minus(a_0, b_0) => { calculator(a_0) - calculator(b_0) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(m) => { if (eval(m)) false else true }
          case AndAlso(m, n) => { if (eval(m) && eval(n)) true else false }
          case OrElse(m, n) => { if (eval(m) || eval(n)) true else false }
          case Imply(m, n) => { if (eval(m) && not(eval(n))) false else true }
          case Equal(m, n) => {
            if (calculator(m) == calculator(n)) true else false
          }
        }
    }
  )
      
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}