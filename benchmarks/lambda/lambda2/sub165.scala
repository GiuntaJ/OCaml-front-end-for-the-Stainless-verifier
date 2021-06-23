import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub165 {
  
        sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
                      val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def find(e, env) = {
            e match {
              case V(n) => {
                env match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (n == hd) true else find(e, tl) }
                }
              }
              case P(v, f) => { find(f, List(v) ++ env) }
              case C(f1, f2) => {
                if (find(f1, env) && find(f2, env)) true else false
              }
            }
          }
          find(exp, Nil())
        }
    }
  )
}