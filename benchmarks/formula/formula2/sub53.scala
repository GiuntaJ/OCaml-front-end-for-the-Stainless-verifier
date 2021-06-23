import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub53 {
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
          case Not(for_val) => { not(eval(for_val)) }
          case AndAlso(left, right) => { eval(left) && eval(right) }
          case OrElse(left, right) => { eval(left) || eval(right) }
          case Imply(left, right) => {
            if (eval(left) == true) eval(right) eq true else true
          }
          case Equal(left, right) => {
            val _2 = {
              def eval_exp: Exp => Int63 = (
                (x) =>
                  {
                    x match {
                      case Num(num_value) => { num_value }
                      case Plus(left, right) => {
                        eval_exp(left) + eval_exp(right)
                      }
                      case Minus(left, right) => {
                        eval_exp(left) - eval_exp(right)
                      }
                    }
                }
              )
              eval_exp(left) eq eval_exp(right)
            }
          }
        }
    }
  )
}