import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub164 {
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
    
  def cal(f) = {
    f match {
      case Num(x) => { x }
      case Plus(a, b) => { cal(a) + cal(b) }
      case Minus(a, b) => { cal(a) - cal(b) }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { if (eval(a)) false else true }
          case AndAlso(b, c) => { eval(b) && eval(c) }
          case OrElse(d, e) => { eval(d) || eval(e) }
          case Imply(f, g) => {
            if (eval(f) == true && eval(g) == false) false else true
          }
          case Equal(h, k) => { if (cal(h) == cal(k)) true else false }
        }
    }
  )
    
  eval(Imply(Imply(Not(True_), AndAlso(False_, OrElse(True_, False_))), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
    
      
}