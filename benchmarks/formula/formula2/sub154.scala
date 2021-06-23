import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub154 {
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
  
  def math: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(e1) => { e1 }
          case Plus(e1, e2) => { math(e1) + math(e2) }
          case Minus(e1, e2) => { math(e1) - math(e2) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(f1) => { if (f1 == True_) false else true }
          case AndAlso(f1, f2) => {
            if (f1 == True_ && f2 == True_) true else false
          }
          case OrElse(f1, f2) => {
            if (f1 == False_ && f2 == False_) false else true
          }
          case Imply(f1, f2) => {
            
              if (
                f1 == False_
              ) {
                true 
              } else if (
                f2 == True_
              ) {
                true 
              } else {
                false
              }
          }
          case Equal(e1, e2) => { if (math(e1) == math(e2)) true else false }
        }
    }
  )
                      
                      
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
}