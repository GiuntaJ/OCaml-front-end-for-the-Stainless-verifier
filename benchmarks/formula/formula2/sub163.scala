import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub163 {
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
  
  def ari(expr: Exp): Int63 = {
    expr match {
      case Num(a) => { a }
      case Plus(c, d) => { ari(c) + ari(d) }
      case Minus(c, d) => { ari(c) - ari(d) }
    }
  }
  
  Equal(Num(1), Num(2))
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f_0) => { if (eval(f_0)) false else true }
          case AndAlso(g, h) => { if (eval(g) && eval(h)) true else false }
          case OrElse(g, h) => { if (eval(g) || eval(h)) true else false }
          case Imply(g, h) => {
            (eval(g), eval(h)) match {
              case (true, false) => { false }
              case (_, _) => { true }
            }
          }
          case Equal(e, x) => { if (ari(e) == ari(x)) true else false }
        }
    }
  )
                   
  
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}