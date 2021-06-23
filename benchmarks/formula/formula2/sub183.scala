import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub183 {
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
  
                    
  def c2N: Exp => Int63 = (
    (exp) =>
      {
        exp match {
          case Num(n_0) => { n_0 }
          case Plus(x_0, y_0) => { c2N(x_0) + c2N(y_0) }
          case Minus(x_0, y_0) => { c2N(x_0) - c2N(y_0) }
        }
    }
  )
        
  val reverse: Boolean => Formula = ( (x) => { if (x == true) False_ else True_ } )
    
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a_0) => { eval(reverse(eval(a_0))) }
          case Equal(a_0, b_0) => { if (c2N(a_0) == c2N(b_0)) true else false }
          case Imply(a_0, b_0) => {
            if (eval(a_0) == true && eval(b_0) == false) false else true
          }
          case AndAlso(a_0, b_0) => { eval(a_0) && eval(b_0) }
          case OrElse(a_0, b_0) => { eval(a_0) || eval(b_0) }
        }
    }
  )
      
  eval(Equal(Plus(Num(1), Num(1)), Minus(Num(4), Num(2))))
  eval(Imply(Imply(True_, False_), True_))
}