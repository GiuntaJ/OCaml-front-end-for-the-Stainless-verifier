import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub113 {
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
  
  def eval_exp: Exp => Int63 = (
    (exp) =>
      {
        exp match {
          case Num(n) => { n }
          case Plus(n1, n2) => { eval_exp(n1) + eval_exp(n2) }
          case Minus(n1, n2) => { eval_exp(n1) - eval_exp(n2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { not(eval(f1)) }
          case AndAlso(f1, f2) => { eval(f1) && eval(f2) }
          case OrElse(f1, f2) => { eval(f1) || eval(f2) }
          case Imply(f1, f2) => {
            (eval(f1), eval(f2)) match {
              case (true, false) => { false }
              case _ => { true }
            }
          }
          case Equal(e1, e2) => {
            if (eval_exp(e1) == eval_exp(e2)) true else false
          }
        }
    }
  )
}