import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub188 {
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
    
  def andAlso(((x, y))) = { if (x == true && y == true) true else false }
  
  def orElse(((x, y))) = { if (x == false && y == false) false else true }
  
  def imply(((x, y))) = {
    
      if (
        x == false && y == true
      ) {
        true 
      } else if (
        x == false && y == false
      ) {
        true 
      } else if (
        x == true && y == false
      ) {
        false 
      } else {
        true
      }
  }
    
  def equal(((x, y))) = { if (x == y) true else false }
    
  def cal(x: Exp): Int63 = {
    x match {
      case Num(n) => { n }
      case Plus(Num(a), Num(b)) => { a + b }
      case Minus(Num(a), Num(b)) => { a - b }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case AndAlso(x, y) => {
            
              if (
                x == True_ && y == True_
              ) {
                true 
              } else if (
                x == False_ || y == False_
              ) {
                false 
              } else {
                andAlso(eval(x), eval(y))
              }
          }
          case OrElse(x, y) => {
            
              if (
                x == False_ && y == False_
              ) {
                false 
              } else if (
                x == True_ || y == True_
              ) {
                true 
              } else {
                orElse(eval(x), eval(y))
              }
          }
          case Imply(x, y) => {
            
              if (
                x == False_ && y == True_
              ) {
                false 
              } else if (
                x == False_ && y == False_
              ) {
                true 
              } else if (
                x == True_ && y == False_
              ) {
                false 
              } else if (
                x == True_ && y == True_
              ) {
                true 
              } else {
                imply(eval(x), eval(y))
              }
          }
          case Not(x) => {
            
              if (
                x == True_
              ) {
                false 
              } else if (
                x == False_
              ) {
                true 
              } else {
                not(eval(x))
              }
          }
          case True_ => { true }
          case False_ => { false }
          case Equal(x, y) => { if (x == y) true else equal(cal(x), cal(y)) }
        }
    }
  )
    
  
    eval(Imply(Imply(True_, False_), True_))
    eval(Equal(Num(1), Plus(Num(1), Num(2))))
  
  
  
  /*
  Write the function
  eval : formula -> bool
  that computes the truth value of a given formula. 
  
  For example,
  eval (Imply (Imply (True,False), True))
  evaluates to true, and
  
  eval (Equal (Num 1, Plus (Num 1, Num 2)))
  evaluates to false
  */
}