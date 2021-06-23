import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub90 {
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
  
  def sub_fun: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(i) => { i }
          case Plus(e1, e2) => { sub_fun(e1) + sub_fun(e2) }
          case Minus(e1, e2) => { sub_fun(e1) - sub_fun(e2) }
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
          case AndAlso(a1, a2) => { if (eval(a1) && eval(a2)) true else false }
          case OrElse(a1, a2) => { if (eval(a1) || eval(a2)) true else false }
          case Imply(a1, a2) => { if (eval(a1) && eval(Not(a2))) false else true
          }
          case Equal(e1, e2) => {
            if (sub_fun(e1) == sub_fun(e2)) true else false
          }
        }
    }
  )
  
}
