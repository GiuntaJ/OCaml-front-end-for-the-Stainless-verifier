import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub142 {
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
            (e) =>
              {
                e match {
                  case Num(a) => { a }
                  case Plus(a, b) => { exptoint(a) + exptoint(b) }
                  case Minus(a, b) => { exptoint(a) - exptoint(b) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(x) => { if (x == True_) false else true }
            case AndAlso(x, y) => { eval(x) && eval(y) }
            case OrElse(x, y) => { eval(x) || eval(y) }
            case Imply(x, y) => {
              val _7 = {
                val imply: (Boolean, Boolean) => Boolean = {
                  case (a, b) =>
                    { if (a == true && b == false) false else true
                  }
                }
                imply(eval(x), eval(y))
              }
            }
            case Equal(x, y) => {
              if (exptoint(x) == exptoint(y)) true else false
            }
          }
        }
    }
  )
  
  
  /*
  When evaluating "Equal(x, y)," it's much more convenient and easier to first convert the Num values into their corresponding
  integer values, perform arithmetic operations on those integers, and compare the final Num values rather than to try to use the 
  Num types themselves and write a more complicated function. Function [exptoint] performs this task.
  */
  
  
  
  
  
}
