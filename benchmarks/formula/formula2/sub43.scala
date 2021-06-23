import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub43 {
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
        val _4 = {
          def foldeval(b) = {
            b match {
              case Num(i) => { i }
              case Plus(l, m) => { foldeval(l) + foldeval(m) }
              case Minus(l, m) => { foldeval(l) - foldeval(m) }
            }
          }
          val _5 = {
            def fold(a) = {
              a match {
                case True_ => { True_ }
                case False_ => { False_ }
                case Not(x1) => { if (fold(x1) == True_) False_ else True_ }
                case AndAlso(x2, y2) => {
                  if (fold(x2) == True_ && fold(y2) == True_) True_ else False_
                }
                case OrElse(x3, y3) => {
                  if (fold(x3) == True_ || fold(y3) == True_) True_ else False_
                }
                case Imply(x4, y4) => {
                  
                    if (
                      fold(x4) == False_
                    ) {
                      True_ 
                    } else if (
                      y4 == True_
                    ) {
                      True_ 
                    } else {
                      False_
                    }
                }
                case Equal(l5, m5) => {
                  if (foldeval(l5) == foldeval(m5)) True_ else False_
                }
              }
            }
            if (fold(f) == True_) true else false
          }
        }
    }
  )
}