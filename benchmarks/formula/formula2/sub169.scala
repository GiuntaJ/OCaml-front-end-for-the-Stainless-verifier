import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub169 {
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
    
    def js: Exp => Int63 = (
    (f) =>
      {
        f match {
          case Num(n) => { n }
          case Plus(a1, a2) => { js(a1) + js(a2) }
          case Minus(a1, a2) => { js(a1) - js(a2) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        f match {
          case True_ => { true }
          case False_ => { false }
          case Not(a) => { if (a == True_) false else true }
          case AndAlso(a1, a2) => {
            if (a1 == True_ && a2 == True_) true else false
          }
          case OrElse(a1, a2) => {
            if (a1 == True_ || a2 == True_) true else false
          }
          case Imply(a1, a2) => {
            if (a1 == True_ && a2 == False_) false else true
          }
          case Equal(a1, a2) => { if (js(a1) == js(a2)) true else false }
        }
    }
  )  
    
    
  
    
    /*TODO*/
  eval(Imply(Imply(True_, False_), True_))
  eval(Equal(Num(1), Plus(Num(1), Num(2))))
  eval(Equal(Num(3), Plus(Num(1), Num(2))))
}