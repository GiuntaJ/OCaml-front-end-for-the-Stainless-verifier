import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub145 {
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
        val _4 = {
          def pre(a) = { if (eval(a) == true) True_ else False_ }
          val _5 = {
            def prem(a) = {
              a match {
                case Num(b) => { Num(b) }
                case Plus(Num(b), Num(c)) => { Num(b + c) }
                case Minus(Num(b), Num(c)) => { Num(b - c) }
                case Plus(b, c) => { prem(Plus(prem(b), prem(c))) }
                case Minus(b, c) => { prem(Minus(prem(b), prem(c))) }
              }
            }
            f match {
              case True_ => { true }
              case False_ => { false }
              case Not(a) => {
                
                  if (
                    a == True_
                  ) {
                    false 
                  } else if (
                    a == False_
                  ) {
                    true 
                  } else {
                    not(eval(a))
                  }
              }
              case AndAlso(a, b) => {
                
                  if (
                    a == True_ && b == True_
                  ) {
                    true 
                  } else if (
                    a == False_ && b == True_
                  ) {
                    false 
                  } else if (
                    a == True_ && b == False_
                  ) {
                    false 
                  } else if (
                    a == False_ && b == False_
                  ) {
                    false 
                  } else {
                    eval(AndAlso(pre(a), pre(b)))
                  }
              }
              case OrElse(a, b) => {
                
                  if (
                    a == True_ && b == True_
                  ) {
                    true 
                  } else if (
                    a == False_ && b == True_
                  ) {
                    true 
                  } else if (
                    a == True_ && b == False_
                  ) {
                    true 
                  } else if (
                    a == False_ && b == False_
                  ) {
                    false 
                  } else {
                    eval(AndAlso(pre(a), pre(b)))
                  }
              }
              case Imply(a, b) => {
                
                  if (
                    a == True_ && b == True_
                  ) {
                    true 
                  } else if (
                    a == False_ && b == True_
                  ) {
                    true 
                  } else if (
                    a == True_ && b == False_
                  ) {
                    false 
                  } else if (
                    a == False_ && b == False_
                  ) {
                    true 
                  } else {
                    eval(Imply(pre(a), pre(b)))
                  }
              }
              case Equal(a, b) => { if (prem(a) == prem(b)) true else false }
            }
          }
        }
    }
  )
  
  
  eval(Not(Not(Not(False_))))
  eval(AndAlso(True_, Not(False_)))
  eval(AndAlso(False_, False_))
  eval(AndAlso(True_, False_))
  eval(OrElse(True_, Not(False_)))
  eval(OrElse(False_, False_))
  eval(OrElse(True_, False_))
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(1))))
  eval(Equal(Num(2), Minus(Num(3), Num(1))))
}