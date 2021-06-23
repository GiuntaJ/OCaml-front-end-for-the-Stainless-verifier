import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub88 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def envcheck: (Exp, List[Exp]) => Boolean = {
            case (exp, li) =>
              {
                exp match {
                  case V(x) => {
                    li match {
                      case Nil() => { false }
                      case Cons(h, t) => {
                        if (h == V(x)) true else envcheck(V(x), t)
                      }
                    }
                  }
                  case P(v, e) => { envcheck(e, V(v) :: li) }
                  case C(e1, e2) => { envcheck(e1, li) && envcheck(e2, li) }
                }
            }
          }
          envcheck(exp, Nil())
        }
    }
  )
}