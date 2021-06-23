import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub203 {
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
  
  
  def cal: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(n) => { n }
          case Plus(g, h) => { cal(g) + cal(h) }
          case Minus(g, h) => { cal(g) - cal(h) }
        }
    }
  )
            
  val eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(h) => { if (h eq True_) false else true }
          case AndAlso(g, h) => { if (g eq True_ && h eq True_) true else false
          }
          case OrElse(g, h) => { if (g eq False_ && h eq False_) false else true
          }
          case Imply(g, h) => { if (g eq True_ && h eq False_) false else true }
          case Equal(g, h) => { if (cal(g) eq cal(h)) true else false }
        }
    }
  )
            
  
            
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}
