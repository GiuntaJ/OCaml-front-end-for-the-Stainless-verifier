import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub19 {
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
    (ex) =>
      {
        ex match {
          case Num(n) => { n }
          case Plus(n1, n2) => { exp_to_int(n1) + exp_to_int(n2) }
          case Minus(n1, n2) => { exp_to_int(n1) - exp_to_int(n2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { not(eval(a)) }
          case AndAlso(a, b) => { if (eval(a)) eval(b) else false }
          case OrElse(a, b) => { if (eval(a)) true else eval(b) }
          case Imply(a, b) => { if (eval(a)) eval(b) else true }
          case Equal(a, b) => {
            if (exp_to_int(a) == exp_to_int(b)) true else false
          }
        }
    }
  )
}