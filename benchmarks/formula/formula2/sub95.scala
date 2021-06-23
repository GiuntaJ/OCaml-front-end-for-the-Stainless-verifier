import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub95 {
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
  	
  	
  def evalHelper1: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(a) => { a }
          case Minus(a, b) => { evalHelper1(a) - evalHelper1(b) }
          case Plus(a, b) => { evalHelper1(a) + evalHelper1(b) }
        }
    }
  )
  	
  def evalHelper: Formula => Formula = (
    (f) =>
      {
        f match {
          case True_ => { True_ }
          case False_ => { False_ }
          case Not(a) => { if (a == True_) False_ else True_ }
          case AndAlso(a, b) => {
            
              if (
                evalHelper(a) == True_ && evalHelper(b) == True_
              ) {
                True_ 
              } else {
                False_
              }
          }
          case OrElse(a, b) => {
            
              if (
                evalHelper(a) == False_ && evalHelper(b) == False_
              ) {
                False_ 
              } else {
                True_
              }
          }
          case Imply(a, b) => {
            
              if (
                evalHelper(a) == True_ && evalHelper(b) == False_
              ) {
                False_ 
              } else {
                True_
              }
          }
          case Equal(a, b) => {
            if (evalHelper1(a) == evalHelper1(b)) True_ else False_
          }
        }
    }
  )
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case f => { eval(evalHelper(f)) }
        }
    }
  )
  
  
  
}
