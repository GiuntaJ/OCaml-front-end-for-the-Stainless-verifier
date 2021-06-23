import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub80 {
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
          case Not(ev) => { if (ev == True_) false else true }
          case AndAlso(ev1, ev2) => {
            if (ev1 == True_ && ev2 == True_) true else false
          }
          case OrElse(ev1, ev2) => {
            if (ev1 == False_ && ev2 == False_) false else true
          }
          case Imply(ev1, ev2) => {
            if (ev1 == True_ && ev2 == False_) false else true
          }
          case Equal(in1, in2) => {
            val _2 = {
              def pl_mi(x) = {
                x match {
                  case Num(n) => { n }
                  case Plus(n1, n2) => { pl_mi(n1) + pl_mi(n2) }
                  case Minus(n1, n2) => { pl_mi(n1) - pl_mi(n2) }
                }
              }
              if (pl_mi(in1) == pl_mi(in2)) true else false
            }
          }
        }
    }
  )
}