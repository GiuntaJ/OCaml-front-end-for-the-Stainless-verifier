import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub180 {
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
  
  def expo: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(a) => { a }
          case Plus(a, b) => { expo(a) + expo(b) }
          case Minus(a, b) => { expo(a) - expo(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { not(eval(a)) }
          case AndAlso(a, b) => { eval(a) && eval(b) }
          case OrElse(a, b) => { eval(a) || eval(b) }
          case Imply(a, b) => { if (eval(a)) eval(b) else true }
          case Equal(a, b) => { expo(a) == expo(b) }
        }
    }
  )
  
    
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
  
    
    
    
    
}