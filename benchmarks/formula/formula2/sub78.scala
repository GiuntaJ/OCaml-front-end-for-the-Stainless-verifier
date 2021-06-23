import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub78 {
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
  
  def eval: Formula => Boolean = ( (f) => { true } )
  
  def eval(f: Formula): Boolean = {
    val _2 = {
      def expin(f) = {
        f match {
          case Num(n) => { n }
          case Plus(e1, e2) => { expin(e1) + expin(e2) }
          case Minus(e1, e2) => { expin(e1) - expin(e2) }
        }
      }
      f match {
        case True_ => { true }
        case False_ => { false }
        case Not(f1) => { if (f1 == True_) false else true }
        case AndAlso(f1, f2) => {
          if (f1 == True_ && f2 == True_) true else false
        }
        case OrElse(f1, f2) => { if (f1 == True_ || f2 == True_) true else false
        }
        case Imply(f1, f2) => { if (f1 == True_ && f2 == False_) false else true
        }
        case Equal(e1, e2) => { if (expin(e1) == expin(e2)) true else false }
      }
    }
  }
}
