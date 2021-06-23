import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub143 {
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
    
  def value: Exp => Int63 = (
    (n) =>
      {
        n match {
          case Num(h) => { h }
          case Plus(x, y) => { value(x) + value(y) }
          case Minus(x, y) => { value(x) * value(y) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(h) => { not(eval(h)) }
          case AndAlso(x, y) => { eval(x) && eval(y) }
          case OrElse(x, y) => { eval(x) || eval(y) }
          case Imply(x, y) => { if (eval(x) == true) eval(y) else true }
          case Equal(x, y) => { if (value(x) == value(y)) true else false }
        }
    }
  )
      
}