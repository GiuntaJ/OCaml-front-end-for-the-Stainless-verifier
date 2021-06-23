import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub84 {
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
  
  def eval: Formula => Boolean = val _0 = {
    def expcall: Exp => Int63 = (
      (e) =>
        {
          e match {
            case Num(x) => { x }
            case Plus(x, y) => { expcall(x) + expcall(y) }
            case Minus(x, y) => { expcall(x) - expcall(y) }
          }
      }
    )
    (
      (f) =>
        {
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(x) => { not(eval(x)) }
            case AndAlso(x, y) => { eval(x) && eval(y) }
            case OrElse(x, y) => { eval(x) || eval(y) }
            case Imply(x, y) => {
              if (eval(x) eq true && eval(y) eq false) false else true
            }
            case Equal(x, y) => { if (expcall(x) eq expcall(y)) true else false
            }
          }
      }
    )
  }
}
