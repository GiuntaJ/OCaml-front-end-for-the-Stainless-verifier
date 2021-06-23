import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub28 {
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
  
  def exptoint: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(a) => { a }
          case Plus(a, b) => { exptoint(a) + exptoint(b) }
          case Minus(a, b) => { exptoint(a) - exptoint(b) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { if (f1 eq True_) false else true }
          case AndAlso(f1, f2) => {
            if (f1 eq True_ && f2 eq True_) true else false
          }
          case OrElse(f1, f2) => {
            if (AndAlso(f1, f2) eq False_) false else true
          }
          case Imply(f1, f2) => {
            if (Not(f1) eq True_ || AndAlso(f1, f2) eq True_) true else false
          }
          case Equal(e1, e2) => {
            if (exptoint(e1) eq exptoint(e2)) true else false
          }
        }
    }
  )
  
  
  
}
