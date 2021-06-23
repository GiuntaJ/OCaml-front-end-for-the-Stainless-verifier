import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub47 {
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
  
  
  
  def expformula: Exp => Int63 = (
    (exp) =>
      {
        exp match {
          case Num(exp) => { exp }
          case Plus(exp1, exp2) => { expformula(exp1) + expformula(exp2) }
          case Minus(exp1, exp2) => { expformula(exp1) - expformula(exp2) }
        }
    }
  )
    
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(fm) => { not(eval(fm)) }
          case AndAlso(fm1, fm2) => { eval(fm1) && eval(fm2) }
          case OrElse(fm1, fm2) => { eval(fm1) || eval(fm2) }
          case Imply(fm1, fm2) => {
            
              if (
                eval(fm1) == false
              ) {
                true 
              } else if (
                eval(fm2) == true
              ) {
                true 
              } else {
                false
              }
          }
          case Equal(exp1, exp2) => {
            if (expformula(exp1) == expformula(exp2)) true else false
          }
        }
    }
  )
  
  
}
