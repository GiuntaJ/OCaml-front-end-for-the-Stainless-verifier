import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub32 {
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
  
  def expfun: Exp => Int63 = (
    (q) =>
      {
        q match {
          case Num(i) => { i }
          case Plus(i, k) => { expfun(i) + expfun(k) }
          case Minus(i, k) => { expfun(i) - expfun(k) }
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
            if (f1 == True_ && f2 eq True_) true else false
          }
          case OrElse(f1, f2) => {
            if (f1 == False_ && f2 eq False_) false else true
          }
          case Imply(f1, f2) => {
            if (f1 == True_ && f2 eq False_) false else true
          }
          case Equal(q1, q2) => { if (expfun(q1) eq expfun(q2)) true else false
          }
        }
    }
  )  
}