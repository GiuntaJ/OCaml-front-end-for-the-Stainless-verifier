import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub82 {
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
  
  def evalexp(exp: Exp): Int63 = {
    exp match {
      case Num(n) => { n }
      case Plus(n1, n2) => {
        (n1, n2) match {
          case (_, _) => { evalexp(n1) + evalexp(n2) }
        }
      }
      case Minus(n1, n2) => {
        (n1, n2) match {
          case (_, _) => { evalexp(n1) - evalexp(n2) }
        }
      }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f_0) => {
            f_0 match {
              case True_ => { false }
              case False_ => { true }
              case _ => { eval(f_0) }
            }
          }
          case AndAlso(f1_0, f2_0) => {
            (f1_0, f2_0) match {
              case (True_, True_) => { true }
              case (False_, _) => { false }
              case (_, False_) => { false }
              case (_, _) => { eval(f1_0) && eval(f2_0) }
            }
          }
          case OrElse(f1_0, f2_0) => {
            (f1_0, f2_0) match {
              case (True_, True_) => { true }
              case (True_, False_) => { true }
              case (False_, True_) => { true }
              case (False_, False_) => { false }
              case (_, _) => { eval(f1_0) || eval(f2_0) }
            }
          }
          case Imply(f1_0, f2_0) => {
            (f1_0, f2_0) match {
              case (True_, True_) => { true }
              case (True_, False_) => { false }
              case (False_, True_) => { true }
              case (False_, False_) => { true }
              case (_, _) => { eval(Not(f1_0)) || eval(f2_0) }
            }
          }
          case Equal(exp1, exp2) => {
            (evalexp(exp1), evalexp(exp2)) match {
              case (a, b) => { if (a == b) true else false }
            }
          }
        }
    }
  )
}