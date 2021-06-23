import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub94 {
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
          case Not(v) => {
            val _19 = {
              val e = eval(v)
              if (e == true) false else true
            }
          }
          case AndAlso(v1, v2) => {
            val _15 = {
              val e1 = eval(v1)
              val _16 = {
                val e2 = eval(v2)
                if (e1 == true && e2 == true) true else false
              }
            }
          }
          case OrElse(v1, v2) => {
            val _11 = {
              val e1 = eval(v1)
              val _12 = {
                val e2 = eval(v2)
                if (e1 == false && e2 == false) false else true
              }
            }
          }
          case Imply(v1, v2) => {
            val _7 = {
              val e1 = eval(v1)
              val _8 = {
                val e2 = eval(v2)
                if (e1 == true && e2 == false) false else true
              }
            }
          }
          case Equal(v1, v2) => {
            val _2 = {
              def eval2: Exp => Int63 = (
                (g) =>
                  {
                    g match {
                      case Num(s) => { s }
                      case Plus(s1, s2) => { eval2(s1) + eval2(s2) }
                      case Minus(s1, s2) => { eval2(s1) - eval2(s2) }
                    }
                }
              )
              val _3 = {
                val e1 = eval2(v1)
                val _4 = {
                  val e2 = eval2(v2)
                  if (e1 == e2) true else false
                }
              }
            }
          }
        }
    }
  )
  
}
