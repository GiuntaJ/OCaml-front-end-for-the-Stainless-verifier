import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub107 {
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
  
  def cal(num: Exp): Exp = {
    num match {
      case Num(i) => { Num(i) }
      case Plus(a, b) => {
        (a, b) match {
          case (Num(a2), Num(b2)) => { Num(a2 + b2) }
          case (_, _) => { Plus(cal(a), cal(b)) }
        }
      }
      case Minus(a, b) => {
        (a, b) match {
          case (Num(a2), Num(b2)) => { Num(a2 - b2) }
          case (_, _) => { Minus(cal(a), cal(b)) }
        }
      }
    }
  } 
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { if (eval(x) == true) false else true }
          case AndAlso(x, y) => { eval(x) && eval(y) }
          case OrElse(x, y) => { eval(x) || eval(y) }
          case Imply(x, y) => {
            if (eval(x) == true && eval(y) == false) false else true
          }
          case Equal(x, y) => {
            (x, y) match {
              case (Num(x2), Num(y2)) => { if (x2 == y2) true else false }
              case (_, _) => { eval(Equal(cal(x), cal(y))) }
            }
          }
        }
    }
  )
  
  
  		
  
  
}
