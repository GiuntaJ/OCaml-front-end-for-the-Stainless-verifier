import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub12 {
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
  
  def e2i(e: Exp): Int63 = {
    e match {
      case Num(n) => { n }
      case Plus(e1, e2) => { e2i(e1) + e2i(e2) }
      case Minus(e3, e4) => { e2i(e3) - e2i(e4) }
    }
  } 
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { if (eval(f1) == true) false else true }
          case AndAlso(f2, f3) => { eval(f2) && eval(f3) }
          case OrElse(f4, f5) => { eval(f4) || eval(f5) }
          case Imply(f6, f7) => {
            (eval(f6), eval(f7)) match {
              case (true, false) => { false }
              case _ => { true }
            }
          }
          case Equal(e1, e2) => {
            val _2 = {
              val v1 = e2i(e1)
              val _3 = {
                val v2 = e2i(e2)
                if (v1 == v2) true else false
              }
            }
          }
        }
    }
  )
}