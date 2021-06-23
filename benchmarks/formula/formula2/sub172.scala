import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub172 {
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
          case Not(x) => { eval(if (eval(x) == true) False_ else True_) }
          case AndAlso(x, y) => {
            eval(if (eval(x) == true && eval(y) == true) True_ else False_)
          }
          case OrElse(x, y) => {
            eval(if (eval(x) == true || eval(y) == true) True_ else False_)
          }
          case Imply(x, y) => {
            eval(if (eval(x) == true && eval(y) == false) False_ else True_)
          }
          case Equal(x, y) => { eval(if (ex(x) == ex(y)) True_ else False_) }
        }
    }
  )
  def ex: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Plus(x, y) => { ex(x) + ex(y) }
          case Minus(x, y) => { ex(x) - ex(y) }
          case Num(x) => { x }
        }
    }
  )
    
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
    
    
    
}