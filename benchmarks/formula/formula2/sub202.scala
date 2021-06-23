import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub202 {
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
  
  def imply(x: (Boolean, Boolean)): Boolean = {
    x match {
      case (true, false) => { false }
      case _ => { true }
    }
  }
  
  def not2(x: Boolean): Boolean = {
    x match {
      case true => { false }
      case false => { true }
    }
  }
  
  def and_Also(x: (Boolean, Boolean)): Boolean = {
    x match {
      case (true, true) => { true }
      case _ => { false }
    }
  }
  
  def or_Else(x: (Boolean, Boolean)): Boolean = {
    x match {
      case (false, false) => { false }
      case _ => { true }
    }
  }
  
  def exp: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(n) => { n }
          case Plus(exp1, exp2) => { exp(exp1) + exp(exp2) }
          case Minus(exp1, exp2) => { exp(exp1) - exp(exp2) }
        }
    }
  )
  
   val equal: (Int63, Int63) => Boolean = {
    case (x, y) => { x == y }
  }
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(formula) => { not(eval(formula)) }
          case Imply(formula1, formula2) => {
            imply(eval(formula1), eval(formula2))
          }
          case AndAlso(formula1, formula2) => {
            and_Also(eval(formula1), eval(formula2))
          }
          case OrElse(formula1, formula2) => {
            or_Else(eval(formula1), eval(formula2))
          }
          case Equal(exp1, exp2) => { equal(exp(exp1), exp(exp2)) }
        }
    }
  )
    
  eval(OrElse(Not(True_), Imply(True_, Equal(Num(3), Num(8)))))
    
}