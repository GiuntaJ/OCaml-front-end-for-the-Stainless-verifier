import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub125 {
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
  
  def eval2: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(e1) => { e1 }
          case Plus(e1, e2) => { eval2(e1) + eval2(e2) }
          case Minus(e1, e2) => { eval2(e1) - eval2(e2) }
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
          case AndAlso(formula1, formula2) => { eval(formula1) && eval(formula2)
          }
          case OrElse(formula1, formula2) => { eval(formula1) || eval(formula2)
          }
          case Imply(formula1, formula2) => {
            
              if (
                eval(formula1) == true && eval(formula2) == false
              ) {
                false 
              } else {
                true
              }
          }
          case Equal(exp1, exp2) => {
            if (eval2(exp1) == eval2(exp2)) true else false
          }
        }
    }
  )
}