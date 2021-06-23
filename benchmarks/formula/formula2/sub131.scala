import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub131 {
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
  
  def eval(formula: Formula): Boolean = {
    formula match {
      case True_ => { true }
      case False_ => { false }
      case Not(x) => { if (eval(x) == false) true else false }
      case AndAlso(x, y) => {
        
          if (
            eval(x) == true && eval(y) == true
          ) {
            true 
          } else if (
            eval(x) == true && eval(y) == false
          ) {
            false 
          } else if (
            eval(x) == false && eval(y) == true
          ) {
            false 
          } else {
            false
          }
      }
      case OrElse(x, y) => {
        
          if (
            eval(x) == true && eval(y) == true
          ) {
            true 
          } else if (
            eval(x) == true && eval(y) == false
          ) {
            true 
          } else if (
            eval(x) == false && eval(y) == true
          ) {
            true 
          } else {
            false
          }
      }
      case Imply(x, y) => {
        
          if (
            eval(x) == true && eval(y) == true
          ) {
            true 
          } else if (
            eval(x) == true && eval(y) == false
          ) {
            false 
          } else if (
            eval(x) == false && eval(y) == true
          ) {
            true 
          } else {
            true
          }
      }
      case Equal(x, y) => {
        val _2 = {
          def exp(fxy) = {
            fxy match {
              case Num(n) => { n }
              case Plus(x, y) => { exp(x) + exp(y) }
              case Minus(x, y) => { exp(x) - exp(y) }
            }
          }
          if (exp(x) eq exp(y)) true else false
        }
      }
    }
  }
}