import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub141 {
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
          def cnum(n) = {
            n match {
              case Num(x) => { x }
              case Plus(x, y) => {
                val _11 = {
                  val nx = cnum(x)
                  val _12 = {
                    val ny = cnum(y)
                    nx + ny
                  }
                }
              }
              case Minus(x, y) => {
                val _7 = {
                  val nx = cnum(x)
                  val _8 = {
                    val ny = cnum(y)
                    nx - ny
                  }
                }
              }
            }
          }
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(b) => {
              val _31 = {
                val vb = eval(b)
                if (vb == true) false else true
              }
            }
            case AndAlso(a, b) => {
              val _27 = {
                val va = eval(a)
                val _28 = {
                  val vb = eval(b)
                  if (va == true && vb == true) true else false
                }
              }
            }
            case OrElse(a, b) => {
              val _23 = {
                val va = eval(a)
                val _24 = {
                  val vb = eval(b)
                  if (va == false && vb == false) false else true
                }
              }
            }
            case Imply(a, b) => {
              val _19 = {
                val va = eval(a)
                val _20 = {
                  val vb = eval(b)
                  if (va == true && vb == false) false else true
                }
              }
            }
            case Equal(a, b) => {
              val _15 = {
                val na = cnum(a)
                val _16 = {
                  val nb = cnum(b)
                  if (na == nb) true else false
                }
              }
            }
          }
        }
    }
  )
        
      
}
