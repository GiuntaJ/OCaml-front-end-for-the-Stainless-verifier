import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub48 {
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
  def va(e: Exp): Int63 = {
    e match {
      case Num(k) => { k }
      case Plus(k1, k2) => { va(k1) + va(k2) }
      case Minus(k1, k2) => { va(k1) - va(k2) }
    }
  }
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(k) => { not(eval(k)) }
          case AndAlso(k1, k2) => { if (eval(k1) == false) false else eval(k2) }
          case OrElse(k1, k2) => { if (eval(k1) == true) true else eval(k2) }
          case Imply(k1, k2) => { not(eval(k1)) || eval(k2) }
          case Equal(k1, k2) => { va(k1) == va(k2) }
        }
    }
  )
}