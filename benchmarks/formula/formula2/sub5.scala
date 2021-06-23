import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub5 {
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
  
  def cal_exp(n: Exp): Int63 = {
    n match {
      case Num(a) => { a }
      case Plus(e1, e2) => { cal_exp(e1) + cal_exp(e2) }
      case Minus(e1, e2) => { cal_exp(e1) - cal_exp(e2) }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => {
            val _18 = {
              val v = eval(x)
              v match {
                case true => { eval(False_) }
                case false => { eval(True_) }
              }
            }
          }
          case AndAlso(e1, e2) => {
            val _14 = {
              val v1 = eval(e1)
              val _15 = {
                val v2 = eval(e2)
                (v1, v2) match {
                  case (true, true) => { eval(True_) }
                  case (true, false) => { eval(False_) }
                  case (false, true) => { eval(False_) }
                  case (false, false) => { eval(False_) }
                }
              }
            }
          }
          case OrElse(e1, e2) => {
            val _10 = {
              val v1 = eval(e1)
              val _11 = {
                val v2 = eval(e2)
                (v1, v2) match {
                  case (true, true) => { eval(True_) }
                  case (true, false) => { eval(True_) }
                  case (false, true) => { eval(True_) }
                  case (false, false) => { eval(False_) }
                }
              }
            }
          }
          case Imply(e1, e2) => {
            val _6 = {
              val v1 = eval(e1)
              val _7 = {
                val v2 = eval(e2)
                (v1, v2) match {
                  case (true, true) => { eval(True_) }
                  case (true, false) => { eval(False_) }
                  case (false, true) => { eval(True_) }
                  case (false, false) => { eval(True_) }
                }
              }
            }
          }
          case Equal(e1, e2) => {
            val _2 = {
              val v1 = cal_exp(e1)
              val _3 = {
                val v2 = cal_exp(e2)
                (v1, v2) match {
                  case (v1, v2) => { if (v1 == v2) eval(True_) else eval(False_)
                  }
                }
              }
            }
          }
        }
    }
  )
}