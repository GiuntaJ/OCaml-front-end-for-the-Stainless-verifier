import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub132 {
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
  
  def expf(x: Exp): Int63 = {
    x match {
      case Num(n) => { n }
      case Plus(e1, e2) => { expf(e1) + expf(e2) }
      case Minus(e1, e2) => { expf(e1) - expf(e2) }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(n) => {
            eval(n) match {
              case true => { false }
              case false => { true }
            }
          }
          case AndAlso(n1, n2) => { eval(n1) && eval(n2) }
          case OrElse(n1, n2) => {
            eval(n1) match {
              case true => {
                eval(n2) match {
                  case true => { true }
                  case false => { true }
                }
              }
              case false => {
                eval(n2) match {
                  case true => { true }
                  case false => { false }
                }
              }
            }
          }
          case Imply(n1, n2) => {
            eval(n1) match {
              case true => {
                eval(n2) match {
                  case true => { true }
                  case false => { false }
                }
              }
              case false => {
                eval(n2) match {
                  case true => { true }
                  case false => { true }
                }
              }
            }
          }
          case Equal(n1, n2) => {
            val _2 = {
              val a1 = expf(n1)
              val _3 = {
                val a2 = expf(n2)
                if (a1 eq a2) true else false
              }
            }
          }
        }
    }
  )
}