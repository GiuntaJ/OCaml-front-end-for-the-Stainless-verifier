import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub187 {
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
  
  def plusMinus: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(e) => { e }
          case Plus(a, b) => { plusMinus(a) + plusMinus(b) }
          case Minus(a, b) => { plusMinus(a) - plusMinus(b) }
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
          case Imply(x, y) => {
            if (eval(x) == true && eval(y) == false) false else true
          }
          case Equal(x, y) => {
            if (plusMinus(x) == plusMinus(y)) true else false
          }
        }
    }
  )
}