import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub134 {
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
  
  def exee: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(f) => { f }
          case Plus(f1, f2) => { exee(f1) + exee(f2) }
          case Minus(f1, f2) => { exee(f1) - exee(f2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f) => { not(eval(f)) }
          case AndAlso(n1, n2) => {
            if (eval(n1) == true && eval(n2) == true) true else false
          }
          case OrElse(n1, n2) => {
            if (eval(n1) == false && eval(n2) == false) false else true
          }
          case Imply(n1, n2) => {
            if (eval(n1) == true && eval(n2) == false) false else true
          }
          case Equal(n1, n2) => { exee(n1) == exee(n2) }
        }
    }
  )
  
}
