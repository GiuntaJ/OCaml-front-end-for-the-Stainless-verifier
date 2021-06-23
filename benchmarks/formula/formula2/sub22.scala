import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub22 {
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
  def trans: Exp => Exp = (
    (e1) =>
      {
        e1 match {
          case Plus(Num(a), Num(b)) => { Num(a + b) }
          case Plus(Num(a), e3) => { Plus(Num(a), trans(e3)) }
          case Plus(e2, Num(b)) => { Plus(trans(e2), Num(b)) }
          case Plus(e2, e3) => { Plus(trans(e2), trans(e3)) }
          case Minus(Num(a), Num(b)) => { Num(a - b) }
          case Minus(Num(a), e3) => { Minus(Num(a), trans(e3)) }
          case Minus(e2, Num(b)) => { Minus(trans(e2), Num(b)) }
          case Minus(e2, e3) => { Minus(trans(e2), trans(e3)) }
          case Num(a) => { Num(a) }
        }
    }
  )
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { if (f1 == True_) false else true }
          case AndAlso(f1, f2) => {
            if (f1 == True_ && f2 == True_) true else false
          }
          case OrElse(f1, f2) => {
            if (f1 == False_ && f2 == False_) false else true
          }
          case Imply(f1, f2) => {
            if (f1 == True_ && f2 == False_) false else true
          }
          case Equal(e1, e2) => { if (trans(e1) == trans(e2)) true else false }
        }
    }
  )
  
}
