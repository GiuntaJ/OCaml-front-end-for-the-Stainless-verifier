import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub4 {
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
          case Not(f1) => {
            val _19 = {
              val v1 = eval(f1)
              if (v1 == true) false else true
            }
          }
          case AndAlso(f1, f2) => {
            val _15 = {
              val v1 = eval(f1)
              val _16 = {
                val v2 = eval(f2)
                v1 && v2
              }
            }
          }
          case OrElse(f1, f2) => {
            val _11 = {
              val v1 = eval(f1)
              val _12 = {
                val v2 = eval(f2)
                v1 || v2
              }
            }
          }
          case Imply(f1, f2) => {
            val _7 = {
              val v1 = eval(f1)
              val _8 = {
                val v2 = eval(f2)
                (v1, v2) match {
                  case (true, false) => { false }
                  case _ => { true }
                }
              }
            }
          }
          case Equal(e1, e2) => {
            val _2 = {
              def ev: Exp => Int63 = (
                (a) =>
                  {
                    a match {
                      case Num(c) => { c }
                      case Plus(c1, c2) => { ev(c1) + ev(c2) }
                      case Minus(c1, c2) => { ev(c1) - ev(c2) }
                    }
                }
              )
              val _3 = {
                val v1 = ev(e1)
                val _4 = {
                  val v2 = ev(e2)
                  if (v1 == v2) true else false
                }
              }
            }
          }
        }
    }
  )
  
  
}
