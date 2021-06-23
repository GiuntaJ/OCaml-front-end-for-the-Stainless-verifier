import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub156 {
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
  
  val flip: Boolean => Boolean = (
    (n) =>
      {
        n match {
          case true => { false }
          case false => { true }
        }
    }
  )
  
  def expeval: Exp => Int63 = (
    (n) =>
      {
        n match {
          case Num(x) => { x }
          case Plus(x, y) => { expeval(x) + expeval(y) }
          case Minus(x, y) => { expeval(x) - expeval(y) }
        }
    }
  )
    
  def expequal: (Exp, Exp) => Boolean = {
    case (n, m) => { if (expeval(n) == expeval(m)) true else false }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(x) => { flip(eval(x)) }
          case AndAlso(x, y) => { eval(x) && eval(y) }
          case OrElse(x, y) => { eval(x) || eval(y) }
          case Imply(x, y) => { flip(eval(x)) || eval(y) }
          case Equal(x, y) => { expequal(x, y) }
        }
    }
  )
  
  
}
