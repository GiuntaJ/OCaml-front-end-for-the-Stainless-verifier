import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub152 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  
    def compare: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(x) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (x == hd) true else compare(exp, tl) }
            }
          }
          case P(x, exp1) => { compare(exp1, x :: lst) }
          case C(exp1, exp2) => { compare(exp1, lst) && compare(exp2, lst) }
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
            case P(x, exp1) => { compare(exp1, x :: lst) }
            case C(exp1, exp2) => { compare(exp1, lst) && compare(exp2, lst) }
          }
        }
    }
  )
}
