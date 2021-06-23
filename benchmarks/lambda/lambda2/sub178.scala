import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub178 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def mem: (Var, List[Var]) => Boolean = {
    case (key, lst) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == key) true else false || mem(key, tl)
          }
        }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def ch: (Exp, List[Var]) => Boolean = {
            case (exp, lst) =>
              {
                exp match {
                  case V(x) => { mem(x, lst) }
                  case P(x, p) => { ch(p, x :: lst) }
                  case C(p, q) => { ch(p, lst) && ch(q, lst) }
                }
            }
          }
          ch(exp, Nil())
        }
    }
  )
}