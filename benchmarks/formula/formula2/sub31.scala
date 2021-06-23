import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub31 {
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
  
  def match_exp: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(e1) => { e1 }
          case Plus(e1, e2) => { match_exp(e1) + match_exp(e2) }
          case Minus(e1, e2) => { match_exp(e1) - match_exp(e2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(n) => { not(eval(n)) }
          case AndAlso(fo1, fo2) => { eval(fo1) && eval(fo2) }
          case OrElse(fo1, fo2) => { eval(fo1) || eval(fo2) }
          case Imply(fo1, fo2) => { not(eval(fo1)) || eval(fo2) }
          case Equal(e1, e2) => {
            val _2 = {
              val v1 = match_exp(e1)
              val _3 = {
                val v2 = match_exp(e2)
                if (v1 == v2) true else false
              }
            }
          }
        }
    }
  )
  
  
}
