import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub115 {
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
  
  def evalex(e: Exp): Int63 = {
    e match {
      case Num(n) => { n }
      case Plus(e1, e2) => {
        val _6 = {
          val n1 = evalex(e1)
          val _7 = {
            val n2 = evalex(e2)
            n1 + n2
          }
        }
      }
      case Minus(e1, e2) => {
        val _2 = {
          val n1 = evalex(e1)
          val _3 = {
            val n2 = evalex(e2)
            n1 - n2
          }
        }
      }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(f1) => {
        val _26 = {
          val b1 = eval(f1)
          if (b1 eq true) false else true
        }
      }
      case AndAlso(f1, f2) => {
        val _22 = {
          val b1 = eval(f1)
          val _23 = {
            val b2 = eval(f2)
            b1 && b2
          }
        }
      }
      case OrElse(f1, f2) => {
        val _18 = {
          val b1 = eval(f1)
          val _19 = {
            val b2 = eval(f2)
            b1 || b2
          }
        }
      }
      case Imply(f1, f2) => {
        val _14 = {
          val b1 = eval(f1)
          val _15 = {
            val b2 = eval(f2)
            if (b1 eq true && b2 eq false) false else true
          }
        }
      }
      case Equal(e1, e2) => {
        val _10 = {
          val n1 = evalex(e1)
          val _11 = {
            val n2 = evalex(e2)
            if (n1 eq n2) true else false
          }
        }
      }
    }
  }
}
