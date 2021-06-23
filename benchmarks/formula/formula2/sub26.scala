import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub26 {
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
  	 
  def getInt(f: Exp): Int63 = {
    f match {
      case Num(n) => { n }
      case Plus(n1, n2) => { getInt(n1) + getInt(n2) }
      case Minus(n1, n2) => { getInt(n1) - getInt(n2) }
    }
  }
  
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(a) => { if (eval(a)) false else true }
      case AndAlso(a, b) => {
        if (eval(a)) if (eval(b)) true else false else false
      }
      case OrElse(a, b) => {
        
          if (
            eval(a)
          ) {
            true 
          } else if (
            eval(b)
          ) {
            true 
          } else {
            false
          }
      }
      case Imply(a, b) => { if (eval(a)) if (eval(b)) true else false else true
      }
      case Equal(a, b) => { if (getInt(a) == getInt(b)) true else false }
    }
  }
}