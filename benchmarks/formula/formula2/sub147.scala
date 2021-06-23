import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub147 {
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
    
  def eq: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(a) => { a }
          case Plus(a, b) => { eq(Num(eq(a) + eq(b))) }
          case Minus(a, b) => { eq(Num(eq(a) - eq(b))) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(True_) => { false }
          case Not(False_) => { true }
          case Not(a) => { eval(a) }
          case Equal(a, b) => { if (eq(a) == eq(b)) true else false }
          case Imply(a, b) => {
            (a, b) match {
              case (True_, True_) => { true }
              case (True_, False_) => { false }
              case (False_, True_) => { true }
              case (False_, False_) => { true }
              case (_, _) => {
                eval(
                  Imply(
                    if (eval(a) == true) True_ else False_,
                    if (eval(b) == true) True_ else False_))
              }
            }
          }
          case AndAlso(a, b) => {
            (a, b) match {
              case (True_, True_) => { true }
              case (True_, False_) => { false }
              case (False_, True_) => { false }
              case (False_, False_) => { false }
              case (_, _) => {
                eval(
                  AndAlso(
                    if (eval(a) == true) True_ else False_,
                    if (eval(b) == true) True_ else False_))
              }
            }
          }
          case OrElse(a, b) => {
            (a, b) match {
              case (True_, True_) => { true }
              case (True_, False_) => { true }
              case (False_, True_) => { true }
              case (False_, False_) => { false }
              case (_, _) => {
                eval(
                  OrElse(
                    if (eval(a) == true) True_ else False_,
                    if (eval(b) == true) True_ else False_))
              }
            }
          }
        }
    }
  )
}