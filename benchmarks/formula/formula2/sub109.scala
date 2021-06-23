import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub109 {
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
          case Not(g) => { if (eval(g)) false else true }
          case AndAlso(g, h) => { if (eval(g) && eval(h)) true else false }
          case OrElse(g, h) => {
            if (eval(g) eq false && eval(h) eq false) false else true
          }
          case Imply(g, h) => { if (eval(g) && eval(h) eq false) false else true
          }
          case Equal(exp1, exp2) => {
            val _2 = {
              def eval_exp(e) = {
                e match {
                  case Num(i) => { i }
                  case Plus(i1, i2) => { eval_exp(i1) + eval_exp(i2) }
                  case Minus(i1, i2) => { eval_exp(i1) - eval_exp(i2) }
                }
              }
              if (eval_exp(exp1) eq eval_exp(exp2)) true else false
            }
          }
        }
    }
  )
}