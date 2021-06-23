import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub60 {
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
  
  def exp_to_int: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(n) => { n }
          case Plus(e1, e2) => { exp_to_int(e1) + exp_to_int(e2) }
          case Minus(e1, e2) => { exp_to_int(e1) - exp_to_int(e2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { if (eval(f1)) false else true }
          case AndAlso(f1, f2) => { if (eval(f1)) eval(f2) else false }
          case OrElse(f1, f2) => { if (eval(f1)) true else eval(f2) }
          case Imply(f1, f2) => { eval(OrElse(Not(f1), f2)) }
          case Equal(e1, e2) => { exp_to_int(e1) eq exp_to_int(e2) }
        }
    }
  )
}