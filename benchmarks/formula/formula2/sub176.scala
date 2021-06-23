import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub176 {
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
  
  val proc_imply: (Boolean, Boolean) => Boolean = (
    (imp) =>
      {
        imp match {
          case (true, true) => { true }
          case (true, false) => { false }
          case (false, true) => { true }
          case (false, false) => { true }
        }
    }
  )
      
  def proc_exp: Exp => Int63 = (
    (x) =>
      {
        x match {
          case Plus(k1, k2) => { proc_exp(k1) + proc_exp(k2) }
          case Minus(k1, k2) => { proc_exp(k1) - proc_exp(k2) }
          case Num(k) => { k }
        }
    }
  )
        
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(k) => { not(eval(k)) }
          case AndAlso(k1, k2) => { eval(k1) && eval(k2) }
          case OrElse(k1, k2) => { eval(k1) || eval(k2) }
          case Imply(k1, k2) => { proc_imply(eval(k1), eval(k2)) }
          case Equal(k1, k2) => { proc_exp(k1) eq proc_exp(k2) }
        }
    }
  )
    
  /*eval ((Imply (Imply (True, False), True)));;*/
  /*eval (Not (Equal (Num 2, Plus (Num 1, Num 2))));;*/
}