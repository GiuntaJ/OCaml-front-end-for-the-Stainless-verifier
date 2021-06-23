import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub76 {
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
  
  val andAlso: (Boolean, Boolean) => Boolean = (
    (f) =>
      {
        f match {
          case (true, true) => { true }
          case _ => { false }
        }
    }
  )
  
  val orElse: (Boolean, Boolean) => Boolean = (
    (f) =>
      {
        f match {
          case (false, false) => { false }
          case _ => { true }
        }
    }
  )
  
  val imply: (Boolean, Boolean) => Boolean = (
    (f) =>
      {
        f match {
          case (frue, false) => { false }
          case _ => { true }
        }
    }
  )
  
  def ca: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(a) => { a }
          case Plus(a, b) => { ca(a) + ca(b) }
          case Minus(a, b) => { ca(a) - ca(b) }
        }
    }
  )
  
  
  val equal: (Exp, Exp) => Boolean = (
    (f) =>
      {
        f match {
          case (a, b) => { if (ca(a) == ca(b)) true else false }
        }
    }
  )
  
  
  
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case AndAlso(a, b) => { andAlso(eval(a), eval(b)) }
          case OrElse(a, b) => { orElse(eval(a), eval(b)) }
          case Imply(a, b) => { imply(eval(a), eval(b)) }
          case Equal(a, b) => { equal(a, b) }
          case _ => { assert(false, "Failure with eval requires formula") }
        }
    }
  ) 
}
