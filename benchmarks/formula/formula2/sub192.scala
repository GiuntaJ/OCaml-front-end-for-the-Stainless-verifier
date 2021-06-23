import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub192 {
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
          case Not(f1) => { if (eval(f1) == true) false else true }
          case AndAlso(f1, f2) => {
            if (eval(f1) == true && eval(f2) == true) true else false
          }
          case OrElse(f1, f2) => {
            if (eval(f1) == true || eval(f2) == true) true else false
          }
          case Imply(f1, f2) => {
            if (eval(f1) == true && eval(f2) == false) false else true
          }
          case Equal(e1, e2) => {
            val _2 = {
              def eval_exp: Exp => Int63 = (
                (e) =>
                  {
                    e match {
                      case Num(x) => { x }
                      case Plus(et1, et2) => { eval_exp(et1) + eval_exp(et2) }
                      case Minus(et1, et2) => { eval_exp(et1) - eval_exp(et2) }
                    }
                }
              )
              if (eval_exp(e1) == eval_exp(e2)) true else false
            }
          }
        }
    }
  )
}