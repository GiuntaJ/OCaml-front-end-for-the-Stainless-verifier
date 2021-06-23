import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub139 {
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
          case Not(p) => { if (p == True_) false else true }
          case AndAlso(p, q) => {
            
              if (
                p == False_
              ) {
                false 
              } else if (
                q == False_
              ) {
                false 
              } else {
                true
              }
          }
          case OrElse(p, q) => {
            
              if (
                p == True_
              ) {
                true 
              } else if (
                q == True_
              ) {
                true 
              } else {
                false
              }
          }
          case Imply(p, q) => {
            
              if (
                q == True_
              ) {
                true 
              } else if (
                p == False_
              ) {
                true 
              } else {
                false
              }
          }
          case Equal(n1, n2) => {
            val _2 = {
              def exp_to_int: Exp => Int63 = (
                (e) =>
                  {
                    e match {
                      case Num(n) => { n }
                      case Plus(n1, n2) => { exp_to_int(n1) + exp_to_int(n2) }
                      case Minus(n1, n2) => { exp_to_int(n1) - exp_to_int(n2) }
                    }
                }
              )
              if (exp_to_int(n1) == exp_to_int(n2)) true else false
            }
          }
        }
    }
  )
    
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}