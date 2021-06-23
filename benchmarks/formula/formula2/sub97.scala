import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub97 {
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
          case Not(nf) => { not(eval(nf)) }
          case AndAlso(nf1, nf2) => { eval(nf1) && eval(nf2) }
          case OrElse(nf1, nf2) => { eval(nf1) || eval(nf2) }
          case Imply(nf1, nf2) => { not(eval(nf1)) || eval(nf2) }
          case Equal(nf1, nf2) => {
            val _2 = {
              def calnum(f1) = {
                f1 match {
                  case Num(n) => { n }
                  case Plus(n1, n2) => { calnum(n1) + calnum(n2) }
                  case Minus(n1, n2) => { calnum(n1) - calnum(n2) }
                }
              }
              calnum(nf1) == calnum(nf2)
            }
          }
        }
    }
  )
   
}