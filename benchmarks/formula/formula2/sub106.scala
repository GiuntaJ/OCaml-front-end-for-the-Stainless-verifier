import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub106 {
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
        val _4 = {
          def number: Exp => Int63 = (
            (e) =>
              {
                e match {
                  case Num(n) => { n }
                  case Plus(e1, e2) => { number(e1) + number(e2) }
                  case Minus(e1, e2) => { number(e1) - number(e2) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(f1) => { not(eval(f1)) }
            case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
            case OrElse(f1, f2) => { eval(f1) || eval(f2) }
            case Imply(f1, f2) => {
              if (eval(f1) == true && eval(f2) == false) false else true
            }
            case Equal(e1, e2) => {
              if (number(e1) == number(e2)) true else false
            }
          }
        }
    }
  )
  
  
  
  
  	
  
}
