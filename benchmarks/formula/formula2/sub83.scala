import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub83 {
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
          case Not(f) => { not(eval(f)) }
          case AndAlso(a, b) => {
            
              if (
                eval(a) == false
              ) {
                false 
              } else if (
                eval(b) == false
              ) {
                false 
              } else {
                true
              }
          }
          case OrElse(a, b) => {
            
              if (
                eval(a) == true
              ) {
                true 
              } else if (
                eval(b) == true
              ) {
                true 
              } else {
                false
              }
          }
          case Imply(a, b) => { not(eval(a)) || eval(b) }
          case Equal(a, b) => {
            val _2 = {
              def eval2(e) = {
                e match {
                  case Num(n) => { n }
                  case Plus(a, b) => { eval2(a) + eval2(b) }
                  case Minus(a, b) => { eval2(a) - eval2(b) }
                }
              }
              eval2(a) == eval2(b)
            }
          }
        }
    }
  )
}