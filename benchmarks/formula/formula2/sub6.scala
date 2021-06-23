import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub6 {
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
  
  def expToInt: Exp => Int63 = (
    (exp1) =>
      {
        exp1 match {
          case Num(int1) => { int1 }
          case Plus(e1, e2) => { expToInt(e1) + expToInt(e2) }
          case Minus(e1, e2) => { expToInt(e1) - expToInt(e2) }
        }
    }
  )
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(formula) => { if (eval(formula) == true) false else true }
          case AndAlso(formula1, formula2) => {
            
              if (
                eval(formula1) == true && eval(formula2) == true
              ) {
                true 
              } else {
                false
              }
          }
          case OrElse(formula1, formula2) => {
            
              if (
                eval(formula1) == false && eval(formula2) == false
              ) {
                false 
              } else {
                true
              }
          }
          case Imply(formula1, formula2) => {
            
              if (
                eval(formula1) == false
              ) {
                true 
              } else if (
                eval(formula2) == true
              ) {
                true 
              } else {
                false
              }
          }
          case Equal(exp1, exp2) => {
            if (expToInt(exp1) == expToInt(exp2)) true else false
          }
        }
    }
  )
}