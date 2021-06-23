import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub100 {
  sealed abstract class Exp {}
  case class Num(param0: Int63) extends Exp {}
  case class Plus(param0: Exp,  param1: Exp) extends Exp {}
  case class Minus(param0: Exp,  param1: Exp) extends Exp {} 
  
  sealed abstract class Formula {}
  case object True_ extends Formula {}
  case object False_ extends Formula {}
  case class Not(param0: Formula) extends Formula {}
  case class AndAlso(param0: Formula,  param1: Formula) extends Formula {}
  case class OrElse(param0: Formula,  param1: Formula) extends Formula {}
  case class Imply(param0: Formula,  param1: Formula) extends Formula {}
  case class Equal(param0: Exp,  param1: Exp) extends Formula {}
  
  def exptoint(f) = {
    f match {
      case Num(n) => { Num(n) }
      case Plus(n1, n2) => {
        (n1, n2) match {
          case (Num(e1), Num(e2)) => { Num(e1 + e2) }
          case (n1, n2) => { Plus(exptoint(n1), exptoint(n2)) }
        }
      }
      case Minus(n1, n2) => {
        (n1, n2) match {
          case (Num(e1), Num(e2)) => { Num(e1 - e2) }
          case (n1, n2) => { Minus(exptoint(n1), exptoint(n2)) }
        }
      }
    }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { not(eval(a)) }
          case AndAlso(b1, b2) => { eval(b1) && eval(b2) }
          case OrElse(b1, b2) => { eval(b1) || eval(b2) }
          case Equal(e1, e2) => {
            if (exptoint(e1) == exptoint(e2)) true else false
          }
          case Imply(b1, b2) => {
            if (eval(b1) == true || eval(b2) == false) false else true
          }
        }
    }
  )
  
}
