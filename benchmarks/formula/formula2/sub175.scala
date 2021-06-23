import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub175 {
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
  
  def calc: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(x) => { x }
          case Plus(x, y) => { calc(x) + calc(y) }
          case Minus(x, y) => { calc(x) - calc(y) }
        }
    }
  )
    
  val eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { if (x eq True_) false else true }
          case AndAlso(x, y) => { if (x eq True_ && y eq True_) true else false
          }
          case OrElse(x, y) => { if (x eq True_ || y eq False_) true else false
          }
          case Imply(x, y) => { if (x eq True_ && y eq False_) false else true }
          case Equal(x, y) => { if (calc(x) eq calc(y)) true else false }
        }
    }
  )
    
    eval(Imply(Imply(True_, False_), True_))
    eval(Equal(Num(1), Plus(Num(1), Num(2))))
}