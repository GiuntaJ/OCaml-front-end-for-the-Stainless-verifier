import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub69 {
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
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(k) => { if (eval(k) == eval(True_)) false else true }
          case AndAlso(p, q) => {
            
              if (
                eval(p) == eval(True_) && eval(q) == eval(True_)
              ) {
                true 
              } else {
                false
              }
          }
          case OrElse(p, q) => {
            
              if (
                eval(p) == eval(False_) && eval(q) == eval(False_)
              ) {
                false 
              } else {
                true
              }
          }
          case Imply(p, q) => {
            
              if (
                eval(p) == eval(True_) && eval(q) == eval(False_)
              ) {
                false 
              } else {
                true
              }
          }
          case Equal(p, q) => { if (eval2(p) == eval2(q)) true else false }
        }
    }
  )
  def eval2(g) = {
    g match {
      case Num(i) => { i }
      case Plus(j, k) => { eval2(j) + eval2(k) }
      case Minus(j, k) => { eval2(j) - eval2(k) }
    }
  }
  
    
  
   /* TODO */
}
