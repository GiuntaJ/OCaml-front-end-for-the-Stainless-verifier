import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub171 {
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
    
  def oper: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(a) => { a }
          case Plus(a, b) => { oper(a) + oper(b) }
          case Minus(a, b) => { oper(a) - oper(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { not(eval(x)) }
          case AndAlso(x, y) => { eval(x) && eval(y) }
          case OrElse(x, y) => { eval(x) || eval(y) }
          case Equal(a, b) => { if (oper(a) == oper(b)) true else false }
          case Imply(x, y) => {
            (eval(x), eval(y)) match {
              case (false, false) => { true }
              case (false, true) => { true }
              case (true, false) => { false }
              case (true, true) => { true }
            }
          }
        }
    }
  )
  
  /*
  eval (Imply (Imply (True, False), True));;
  eval (Equal (Num 1, Plus (Num 1, Num 2)));;
  */
}