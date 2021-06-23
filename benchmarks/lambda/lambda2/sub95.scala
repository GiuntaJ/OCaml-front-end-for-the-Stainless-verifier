import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub95 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def scan: (Var, List[Var]) => Boolean = {
    case (s, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == s) true else scan(s, tl) }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def subFun: (Exp, List[Var]) => Boolean = {
            case (e, env) =>
              {
                e match {
                  case V(v) => { if (scan(v, env)) true else false }
                  case P(v, e) => { subFun(e, v :: env) }
                  case C(e1, e2) => {
                    if (subFun(e1, env) && subFun(e2, env)) true else false
                  }
                }
            }
          }
          subFun(exp, Nil())
        }
    }
  )
}