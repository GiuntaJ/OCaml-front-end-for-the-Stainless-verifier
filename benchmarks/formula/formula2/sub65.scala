import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub65 {
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
          case Not(n) => { not(eval(n)) }
          case AndAlso(e1, e2) => { eval(e1) && eval(e2) }
          case OrElse(e1, e2) => { eval(e1) || eval(e2) }
          case Imply(e1, e2) => { if (eval(e1)) eval(e2) else true }
          case Equal(e1, e2) => {
            val _2 = {
              def ex(n) = {
                n match {
                  case Num(k) => { k }
                  case Plus(v1, v2) => { ex(v1) + ex(v2) }
                  case Minus(v1, v2) => { ex(v1) - ex(v2) }
                }
              }
              if (ex(e1) == ex(e2)) true else false
            }
          }
        }
    }
  )
}