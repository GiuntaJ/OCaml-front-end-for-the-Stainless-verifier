import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_formula_formula2_sub112 {
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
          def evalExp: Exp => Int63 = (
            (e) =>
              {
                e match {
                  case Num(n) => { n }
                  case Plus(n1, n2) => { evalExp(n1) + evalExp(n2) }
                  case Minus(n1, n2) => { evalExp(n1) - evalExp(n2) }
                }
            }
          )
          f match {
            case True_ => { true }
            case False_ => { false }
            case Not(f1) => { if (eval(f1) == true) false else true }
            case AndAlso(f1, f2) => {
              if (eval(f1) == true && eval(f2) == true) true else false
            }
            case OrElse(f1, f2) => {
              if (eval(f1) == true || eval(f2) == true) true else false
            }
            case Imply(f1, f2) => { if (eval(f1) == true) eval(f2) else true }
            case Equal(e1, e2) => {
              if (evalExp(e1) == evalExp(e2)) true else false
            }
          }
        }
    }
  ) 
}