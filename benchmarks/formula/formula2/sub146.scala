import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub146 {
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
  
  
  def cal(ex: Exp): Int63 = {
    ex match {
      case Num(num) => { num }
      case Plus(exp1, exp2) => { cal(exp1) + cal(exp2) }
      case Minus(exp1, exp2) => { cal(exp1) - cal(exp2) }
    }
  }
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def evalu(formula) = {
            formula match {
              case True_ => { true }
              case False_ => { false }
              case Not(formul) => {
                evalu(formul) match {
                  case true => { false }
                  case false => { true }
                }
              }
              case AndAlso(formul1, formul2) => {
                evalu(formul1) match {
                  case true => {
                    evalu(formul2) match {
                      case true => { true }
                      case false => { false }
                    }
                  }
                  case false => { false }
                }
              }
              case OrElse(formul1, formul2) => {
                evalu(formul1) match {
                  case true => { true }
                  case false => {
                    evalu(formul2) match {
                      case true => { true }
                      case false => { false }
                    }
                  }
                }
              }
              case Imply(formul1, formul2) => {
                evalu(formul1) match {
                  case true => {
                    evalu(formul2) match {
                      case true => { true }
                      case false => { false }
                    }
                  }
                  case false => { true }
                }
              }
              case Equal(exp1, exp2) => {
                if (cal(exp1) == cal(exp2)) true else false
              }
            }
          }
          evalu(f)
        }
    }
  )
    
    
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
  eval(AndAlso(Equal(Num(1), Plus(Num(1), Num(0))), False_))
}