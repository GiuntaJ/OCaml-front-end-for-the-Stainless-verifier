import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub10 {
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
  
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(a) => { if (eval(a)) false else true }
      case AndAlso(left, right) => {
        if (eval(left) && eval(right)) true else false
      }
      case OrElse(left, right) => {
        if (eval(left) || eval(right)) true else false
      }
      case Imply(left, right) => {
        
          if (
            eval(left) && eval(right)
          ) {
            true 
          } else if (
            eval(left) == true && eval(right) == false
          ) {
            true 
          } else {
            false
          }
      }
      case Equal(left, right) => {
        val _2 = {
          def env(v) = {
            v match {
              case Num(a) => { a }
              case Plus(a, b) => { env(a) + env(b) }
              case Minus(a, b) => { env(a) - env(b) }
            }
          }
          if (env(left) == env(right)) true else false
        }
      }
    }
  }   
  	 
}