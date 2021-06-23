import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub49 {
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
          def change: Exp => Int63 = (
            (ex) =>
              {
                ex match {
                  case Num(a) => { a }
                  case Plus(a, b) => { change(a) + change(b) }
                  case Minus(a, b) => { change(a) - change(b) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(m) => { if (eval(m) == true) false else true }
            case AndAlso(m, n) => {
              if (eval(m) == true && eval(n) == true) true else false
            }
            case OrElse(m, n) => {
              if (eval(m) == true || eval(n) == true) true else false
            }
            case Imply(m, n) => {
              if (eval(m) == true && eval(n) == false) false else true
            }
            case Equal(a, b) => { if (change(a) == change(b)) true else false }
          }
        }
    }
  )
}