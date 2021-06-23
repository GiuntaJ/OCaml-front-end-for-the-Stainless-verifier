import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub86 {
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
  
  def imply(v: (Boolean, Boolean)): Boolean = {
    v match {
      case (true, x) => { x }
      case (false, x) => { true }
    }
  }
  
  def calc(e: Exp): Int63 = {
    e match {
      case Num(n) => { n }
      case Plus(e1, e2) => { calc(e1) + calc(e2) }
      case Minus(e1, e2) => { calc(e1) - calc(e2) }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(fm) => { not(eval(fm)) }
          case AndAlso(fm1, fm2) => { eval(fm1) && eval(fm2) }
          case OrElse(fm1, fm2) => { eval(fm2) || eval(fm2) }
          case Imply(fm1, fm2) => { imply(eval(fm1), eval(fm2)) }
          case Equal(e1, e2) => { calc(e1) == calc(e2) }
        }
    }
  )
}