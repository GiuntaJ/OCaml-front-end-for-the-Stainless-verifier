import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub50 {
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
          case Not(b) => { if (b == True_) eval(True_) else eval(False_) }
          case AndAlso(a, b) => {
            
              if (
                a == False_
              ) {
                eval(False_) 
              } else if (
                b == False_
              ) {
                eval(False_) 
              } else {
                eval(True_)
              }
          }
          case OrElse(a, b) => {
            
              if (
                a == True_
              ) {
                eval(True_) 
              } else if (
                b == True_
              ) {
                eval(True_) 
              } else {
                eval(False_)
              }
          }
          case Imply(a, b) => {
            
              if (
                a == False_
              ) {
                eval(True_) 
              } else if (
                b == True_
              ) {
                eval(True_) 
              } else {
                eval(False_)
              }
          }
          case Equal(a, b) => { if (a == b) eval(True_) else eval(False_) }
        }
    }
  )
}