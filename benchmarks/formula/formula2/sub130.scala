import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub130 {
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
        val _4 = {
          def exptoint: Exp => Int63 = (
            (f1) =>
              {
                f1 match {
                  case Num(fs) => { fs }
                  case Plus(fs, ls) => { exptoint(fs) + exptoint(ls) }
                  case Minus(fs, ls) => { exptoint(fs) - exptoint(ls) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case AndAlso(fs, ls) => {
              
                if (
                  fs == False_
                ) {
                  false 
                } else if (
                  ls == False_
                ) {
                  false 
                } else {
                  true
                }
            }
            case OrElse(fst, lst) => {
              
                if (
                  fst == True_
                ) {
                  true 
                } else if (
                  lst == True_
                ) {
                  true 
                } else {
                  false
                }
            }
            case Imply(fst, lst) => {
              if (fst == True_ && lst == False_) false else true
            }
            case Equal(fs, ls) => {
              if (exptoint(fs) == exptoint(ls)) true else false
            }
            case Not(a1) => { not(eval(a1)) }
          }
        }
    }
  )
}