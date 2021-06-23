import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub122 {
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
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { if (eval(x) == true) false else true }
          case AndAlso(x, y) => {
            (eval(x), eval(y)) match {
              case (true, true) => { true }
              case _ => { false }
            }
          }
          case OrElse(x, y) => {
            (eval(x), eval(y)) match {
              case (false, false) => { false }
              case _ => { true }
            }
          }
          case Imply(x, y) => {
            (eval(x), eval(y)) match {
              case (true, false) => { false }
              case _ => { true }
            }
          }
          case Equal(x, y) => {
            val _2 = {
              def exp_eval(e) = {
                e match {
                  case Num(e1) => { e1 }
                  case Plus(e1, e2) => { exp_eval(e1) + exp_eval(e2) }
                  case Minus(e1, e2) => { exp_eval(e1) - exp_eval(e2) }
                }
              }
              if (exp_eval(x) == exp_eval(y)) true else false
            }
          }
        }
    }
  )
}