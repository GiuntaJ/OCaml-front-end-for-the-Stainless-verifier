import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub41 {
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
          case Not(form) => { if (eval(form) == true) false else true }
          case AndAlso(form1, form2) => {
            if (eval(form1) == true && eval(form2) == true) true else false
          }
          case OrElse(form1, form2) => {
            if (eval(form1) == false && eval(form2) == false) false else true
          }
          case Imply(form1, form2) => {
            if (eval(form1) == true && eval(form2) == false) false else true
          }
          case Equal(ex1, ex2) => {
            val _2 = {
              def eti(e) = {
                e match {
                  case Num(n) => { n }
                  case Plus(n, l) => { eti(n) + eti(l) }
                  case Minus(n, l) => { eti(n) - eti(l) }
                }
              }
              if (eti(ex1) == eti(ex2)) true else false
            }
          }
        }
    }
  )
}