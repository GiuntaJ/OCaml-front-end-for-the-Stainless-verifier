import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub153 {
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
  
  def fnum: Exp => Int63 = (
    (e) =>
      {
        e match {
          case Num(x) => { x }
          case Plus(x, y) => { fnum(x) + fnum(y) }
          case Minus(x, y) => { fnum(x) - fnum(y) }
        }
    }
  )
  
  val eval: Formula => Boolean = (
    (f) =>
      {
        val _4 = {
          def findval: Formula => Boolean = (
            (fm) =>
              {
                fm match {
                  case True_ => { true }
                  case False_ => { false }
                  case Not(x) => { if (findval(x) == true) false else true }
                  case AndAlso(x, y) => {
                    
                      if (
                        findval(x) == true && findval(y) == true
                      ) {
                        true 
                      } else {
                        false
                      }
                  }
                  case OrElse(x, y) => {
                    
                      if (
                        findval(x) == false && findval(y) == false
                      ) {
                        false 
                      } else {
                        true
                      }
                  }
                  case Imply(x, y) => {
                    
                      if (
                        findval(x) == true && findval(y) == false
                      ) {
                        false 
                      } else {
                        true
                      }
                  }
                  case Equal(x, y) => { if (fnum(x) == fnum(y)) true else false
                  }
                }
            }
          )
          findval(f)
        }
    }
  )
    
    eval(Imply(Imply(True_, False_), True_))
    eval(Equal(Num(1), Plus(Num(1), Num(2))))
}