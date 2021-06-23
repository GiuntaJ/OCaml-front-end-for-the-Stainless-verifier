import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub67 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def envcheck: (Exp, List[Exp]) => Boolean = {
            case (e, l) =>
              {
                e match {
                  case V(x) => {
                    val _7 = {
                      def varcheck: List[Exp] => Boolean = (
                        (l2) =>
                          {
                            l2 match {
                              case Nil() => { false }
                              case Cons(h, t) => {
                                if (h == V(x)) true else varcheck(t)
                              }
                            }
                        }
                      )
                      varcheck(l)
                    }
                  }
                  case P(v, e) => { envcheck(e, V(v) :: l) }
                  case C(e1, e2) => {
                    if (envcheck(e1, l) && envcheck(e2, l)) true else false
                  }
                }
            }
          }
          envcheck(e, Nil())
        }
    }
  )
}