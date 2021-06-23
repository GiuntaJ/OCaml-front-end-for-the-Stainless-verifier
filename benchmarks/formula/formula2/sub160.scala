import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub160 {
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
    
  
  
  val eval: Formula => Boolean = (
    (x) =>
      {
        x match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { if (x == True_) false else true }
          case AndAlso(x, y) => { if (x == True_ && y == True_) true else false
          }
          case OrElse(x, y) => { if (x == True_ || y == True_) true else false }
          case Imply(x, y) => { if (x == True_ && y == False_) false else true }
          case Equal(x, y) => { if (x == y) true else false }
        }
    }
  )
    
    
  def cal: Exp => Int63 = (
    (x) =>
      {
        x match {
          case Num(x) => { x }
          case Plus(x, y) => { cal(x) + cal(y) }
          case Minus(x, y) => { cal(x) - cal(y) }
        }
    }
  )
  
  eval(True_)
  eval(AndAlso(True_, False_))
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}