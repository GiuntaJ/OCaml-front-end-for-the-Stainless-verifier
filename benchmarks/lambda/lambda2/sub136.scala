import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub136 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
    def comp: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(x) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (x == hd) true else comp(exp, tl) }
            }
          }
          case P(x, expa) => { comp(expa, x :: lst) }
          case C(expa, expb) => { comp(expa, lst) && comp(expb, lst) }
        }
    }
  }
  
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val lst = Nil()
          exp match {
            case V(x) => { false }
            case P(x, expa) => { comp(expa, x :: lst) }
            case C(expa, expb) => { comp(expa, lst) && comp(expb, lst) }
          }
        }
    }
  )
}
