import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub190 {
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
    (x) =>
      {
        val _4 = {
          def eval_num: Exp => Int63 = (
            (n) =>
              {
                n match {
                  case Num(n) => { n }
                  case Plus(n1, n2) => { eval_num(n1) + eval_num(n2) }
                  case Minus(n1, n2) => { eval_num(n1) - eval_num(n2) }
                }
            }
          )
          x match {
            case True_ => { true }
            case False_ => { false }
            case Not(n) => { if (eval(n) == true) false else true }
            case AndAlso(n1, n2) => {
              if (eval(n1) == true && eval(n2) == true) true else false
            }
            case OrElse(n1, n2) => {
              if (eval(n1) == true || eval(n2) == true) true else false
            }
            case Imply(n1, n2) => {
              if (eval(n1) == true && eval(n2) == false) false else true
            }
            case Equal(n1, n2) => {
              if (eval_num(n1) == eval_num(n2)) true else false
            }
          }
        }
    }
  )
}