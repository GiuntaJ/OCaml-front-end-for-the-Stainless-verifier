import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub191 {
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
  
  def eval_num: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Plus(exp1, exp2) => { eval_num(exp1) + eval_num(exp2) }
          case Minus(exp1, exp2) => { eval_num(exp1) - eval_num(exp2) }
          case Num(a) => { a }
        }
    }
  )
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Imply(f1_0, f2_0) => {
            
              if (
                eval(f1_0) == true
              ) {
                if (eval(f2_0) == true) true else false 
              } else {
                true
              }
          }
          case Not(f_0) => { if (eval(f_0) == true) false else true }
          case AndAlso(f1_0, f2_0) => {
            if (eval(f1_0) == true && eval(f2_0) == true) true else false
          }
          case OrElse(f1_0, f2_0) => {
            if (eval(f1_0) == true || eval(f2_0) == true) true else false
          }
          case Equal(exp1, exp2) => { eval_num(exp1) == eval_num(exp2) }
        }
    }
  )
  
        
        
}