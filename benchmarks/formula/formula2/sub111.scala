import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub111 {
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
          case Not(f) => { not(eval(f)) }
          case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
          case OrElse(f1, f2) => { eval(f1) || eval(f2) }
          case Imply(f1, f2) => {
            if (eval(f1) == true && eval(f2) == false) false else true
          }
          case Equal(e1, e2) => {
            val _2 = {
              def evalNum: Exp => Int63 = (
                (ex) =>
                  {
                    ex match {
                      case Num(n) => { n }
                      case Plus(n1, n2) => { evalNum(n1) + evalNum(n2) }
                      case Minus(n1, n2) => { evalNum(n1) - evalNum(n2) }
                    }
                }
              )
              if (evalNum(e1) == evalNum(e2)) true else false
            }
          }
        }
    }
  )
}