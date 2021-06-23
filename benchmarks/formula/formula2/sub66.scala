import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub66 {
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
  
  def cal(x: Exp): Int63 = {
    x match {
      case Num(n) => { n }
      case Plus(n1, n2) => { cal(n1) + cal(n2) }
      case Minus(n1, n2) => { cal(n1) - cal(n2) }
    }
  }
  	
  	
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(n) => { if (eval(n)) false else true }
          case AndAlso(n1, n2) => { eval(n1) && eval(n2) }
          case OrElse(n1, n2) => { eval(n1) || eval(n2) }
          case Imply(n1, n2) => { if (eval(n1) && not(eval(n2))) false else true
          }
          case Equal(n1, n2) => {
            val _2 = {
              val r1 = cal(n1)
              val _3 = {
                val r2 = cal(n2)
                if (r1 eq r2) true else false
              }
            }
          }
        }
    }
  )
}
