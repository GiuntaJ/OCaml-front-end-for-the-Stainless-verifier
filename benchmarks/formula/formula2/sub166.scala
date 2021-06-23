import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub166 {
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
  
  /*let eval : formula -> bool*/
  /*= fun f -> /*TODO*/*)
  
  def eval_exp: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(a) => { a }
          case Plus(a, b) => { eval_exp(a) + eval_exp(b) }
          case Minus(a, b) => { eval_exp(a) - eval_exp(b) }
        }
    }
  )
  
  
  def eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { if (a == True_) eval(False_) else eval(True_) }
          case AndAlso(a, b) => { eval(a) && eval(b) }
          case OrElse(a, b) => { eval(a) || eval(b) }
          case Imply(a, b) => { eval(Not(a)) || eval(b) }
          case Equal(a, b) => { if (eval_exp(a) == eval_exp(b)) true else false
          }
        }
    }
  )
  
  
      
  eval(True_)
  
  eval(Equal(Num(4), Plus(Num(1), Num(2))))
      
      
      
      
      
}