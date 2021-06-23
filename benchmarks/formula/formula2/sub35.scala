import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub35 {
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
          case Not(inNot) => { if (eval(inNot)) false else true }
          case AndAlso(and1, and2) => {
            if (eval(and1) && eval(and2)) true else false
          }
          case OrElse(or1, or2) => { if (eval(or1) || eval(or2)) true else false
          }
          case Imply(im1, im2) => {
            eval(im1) match {
              case true => { if (eval(im2)) true else false }
              case false => { true }
            }
          }
          case Equal(exp1, exp2) => {
            val _2 = {
              def expcheck: Exp => Int63 = (
                (e) =>
                  {
                    e match {
                      case Num(inNum) => { inNum }
                      case Plus(p1, p2) => { expcheck(p1) + expcheck(p2) }
                      case Minus(m1, m2) => { expcheck(m1) - expcheck(m2) }
                    }
                }
              )
              if (expcheck(exp1) == expcheck(exp2)) true else false
            }
          }
        }
    }
  )
  
}
