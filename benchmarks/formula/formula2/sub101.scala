import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub101 {
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
  
  def calc: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(a) => { a }
          case Plus(a, b) => { calc(a) + calc(b) }
          case Minus(a, b) => { calc(a) - calc(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { if (eval(a)) false else true }
          case AndAlso(a, b) => { if (eval(a) && eval(b)) true else false }
          case OrElse(a, b) => { if (eval(a) || eval(b)) true else false }
          case Imply(a, b) => { if (eval(a) == false) true else eval(b) }
          case Equal(a, b) => { if (calc(a) == calc(b)) true else false }
        }
    }
  )
  
  
}
