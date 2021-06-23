import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub205 {
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
    
  def chlwhd: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(a) => { a }
          case Plus(a, b) => { chlwhd(a) + chlwhd(b) }
          case Minus(a, b) => { chlwhd(a) - chlwhd(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f) => { not(eval(f)) }
          case AndAlso(f_0, f) => { eval(f_0) && eval(f) }
          case OrElse(f_0, f) => { eval(f_0) || eval(f) }
          case Imply(f_0, f) => {
            
              if (
                eval(f_0) == true && eval(f) == false
              ) {
                false 
              } else if (
                eval(f_0) == true && eval(f) == true
              ) {
                true 
              } else if (
                eval(f_0) == false && eval(f) == true
              ) {
                true 
              } else {
                false
              }
          }
          case Equal(a, b) => { if (chlwhd(a) == chlwhd(b)) true else false }
        }
    }
  )
                      
  
  /*
  Write the function
    eval : formula -> bool
  that computes the truth value of a given formula. For example,
    eval (Imply (Imply (True,False), True))
  evaluates to true, and
    eval (Equal (Num 1, Plus (Num 1, Num 2)))
  evaluates to false. */
}