import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub118 {
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
  	
  def getInt: Exp => Int63 = (
    (number) =>
      {
        number match {
          case Num(x) => { x }
          case Plus(a, b) => { getInt(a) + getInt(b) }
          case Minus(a, b) => { getInt(a) - getInt(b) }
        }
    }
  )
  	
  def getExpression: Exp => Exp = (
    (f) =>
      {
        f match {
          case Num(x) => { Num(x) }
          case Plus(a, b) => { Num(getInt(Plus(a, b))) }
          case Minus(a, b) => { Num(getInt(Minus(a, b))) }
        }
    }
  )
  			
  def getValue: Formula => Formula = (
    (f) =>
      {
        f match {
          case True_ => { True_ }
          case False_ => { False_ }
          case Not(x) => {
            
              if (
                x == True_
              ) {
                False_ 
              } else if (
                x == False_
              ) {
                True_ 
              } else {
                getValue(Not(getValue(x)))
              }
          }
          case AndAlso(x, y) => {
            
              if (
                x == True_ && y == True_
              ) {
                True_ 
              } else if (
                x == False_ || y == False_
              ) {
                False_ 
              } else {
                getValue(AndAlso(getValue(x), getValue(y)))
              }
          }
          case OrElse(x, y) => {
            
              if (
                x == True_ && y == False_
              ) {
                True_ 
              } else if (
                x == False_ && y == True_
              ) {
                True_ 
              } else if (
                x == False_ && y == False_
              ) {
                False_ 
              } else if (
                x == True_ && y == True_
              ) {
                False_ 
              } else {
                getValue(OrElse(getValue(x), getValue(y)))
              }
          }
          case Imply(x, y) => {
            
              if (
                x == False_
              ) {
                True_ 
              } else if (
                x == True_ && y == True_
              ) {
                True_ 
              } else if (
                x == True_ && y == False_
              ) {
                False_ 
              } else {
                getValue(Imply(getValue(x), getValue(y)))
              }
          }
          case Equal(ex1, ex2) => {
            if (getExpression(ex1) == getExpression(ex2)) True_ else False_
          }
        }
    }
  )
   			
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        
          if (
            f == True_
          ) {
            true 
          } else if (
            f == False_
          ) {
            false 
          } else {
            eval(getValue(f))
          }
    }
  )
}
