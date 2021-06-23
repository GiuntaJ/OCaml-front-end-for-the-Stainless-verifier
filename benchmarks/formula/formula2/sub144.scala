import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub144 {
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
          case Num(n) => { n }
          case Plus(n1, n2) => { calc(n1) + calc(n2) }
          case Minus(n1, n2) => { calc(n1) - calc(n2) }
        }
    }
  )
    
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(b1) => { if (eval(b1)) false else true }
          case AndAlso(b1, b2) => { eval(b1) && eval(b2) }
          case OrElse(b1, b2) => { eval(b1) || eval(b2) }
          case Imply(b1, b2) => {
            if (eval(b1) == false && eval(b2) == true) false else true
          }
          case Equal(e1, e2) => { calc(e1) == calc(e2) }
        }
    }
  )
}