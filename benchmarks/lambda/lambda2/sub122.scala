import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub122 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def ch: (Exp, List[Var]) => Boolean = {
            case (r, lst) =>
              {
                r match {
                  case V(a) => {
                    lst match {
                      case Nil() => { false }
                      case Cons(m, n) => { if (m == a) true else ch(r, n) }
                    }
                  }
                  case P(a, b) => { ch(b, lst ++ List(a)) }
                  case C(a, b) => { ch(a, lst) && ch(b, lst) }
                }
            }
          }
          ch(exp, Nil())
        }
    }
  )
}