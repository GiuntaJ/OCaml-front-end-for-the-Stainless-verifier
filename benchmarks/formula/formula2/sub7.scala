import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub7 {
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
          case Not(x) => { if (eval(x) == true) false else true }
          case AndAlso(x, y) => {
            
              if (
                eval(x) == false
              ) {
                false 
              } else if (
                eval(y) == false
              ) {
                false 
              } else {
                true
              }
          }
          case OrElse(x, y) => {
            
              if (
                eval(x) == true
              ) {
                true 
              } else if (
                eval(y) == true
              ) {
                true 
              } else {
                false
              }
          }
          case Imply(x, y) => {
            if (eval(x) == true && eval(y) == false) false else true
          }
          case Equal(x, y) => {
            val _2 = {
              def ev: Exp => Int63 = (
                (e) =>
                  {
                    e match {
                      case Num(n) => { n }
                      case Plus(x, y) => { ev(x) + ev(y) }
                      case Minus(x, y) => { ev(x) - ev(y) }
                    }
                }
              )
              if (ev(x) == ev(y)) true else false
            }
          }
        }
    }
  )
  
}
