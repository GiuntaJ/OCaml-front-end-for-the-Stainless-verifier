import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub102 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def matchi: (Exp, List[String]) => Boolean = {
    case (exp, lst) =>
      {
        exp match {
          case V(a) => {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == a) true else matchi(exp, tl) }
            }
          }
          case P(a, b) => { matchi(b, List(a) ++ lst) }
          case C(a, b) => { matchi(a, lst) && matchi(b, lst) }
        }
    }
  }
  
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(a) => { matchi(exp, Nil()) }
          case P(a, b) => { matchi(b, List(a)) }
          case C(a, b) => { matchi(a, Nil()) && matchi(b, Nil()) }
        }
    }
  )
}