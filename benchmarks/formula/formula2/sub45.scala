import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub45 {
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
  
  def evalexp(f: Exp): Int63 = {
    f match {
      case Num(x) => { x }
      case Plus(x, y) => { evalexp(x) + evalexp(y) }
      case Minus(x, y) => { evalexp(x) + evalexp(y) }
    }
  }
  
  def eval(f: Formula): Boolean = {
    f match {
      case True_ => { true }
      case False_ => { false }
      case Not(p) => { if (eval(p) == true) false else true }
      case AndAlso(p, q) => {
        if (eval(p) == true && eval(q) == true) true else false
      }
      case OrElse(p, q) => {
        if (eval(p) == true || eval(q) == true) true else false
      }
      case Imply(p, q) => { if (eval(q) == true) true else false }
      case Equal(p, q) => { if (evalexp(p) == evalexp(q)) true else false }
    }
  }                             
  
  
  
  
     
}