import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub138 {
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
  
  def eval_exp: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(n) => { n }
          case Plus(exp1, exp2) => { eval_exp(exp1) + eval_exp(exp2) }
          case Minus(exp1, exp2) => { eval_exp(exp1) - eval_exp(exp2) }
        }
    }
  )
    
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(formula) => { not(eval(formula)) }
          case AndAlso(formula1, formula2) => { eval(formula1) && eval(formula2)
          }
          case OrElse(formula1, formula2) => { eval(formula1) || eval(formula2)
          }
          case Imply(formula1, formula2) => {
            
              if (
                eval(formula1) == true && eval(formula2) == false
              ) {
                false 
              } else {
                true
              }
          }
          case Equal(exp1, exp2) => {
            if (eval_exp(exp1) == eval_exp(exp2)) true else false
          }
        }
    }
  )
  
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}