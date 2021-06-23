import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub153 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    def g(e, s) = {
    e match {
      case V(a) => {
        val _2 = {
          def f(a, b) = {
            a match {
              case Nil() => { false }
              case Cons(h, t) => { if (h == b) true else f(t, b) }
            }
          }
          if (f(s, a)) true else false
        }
      }
      case P(v, e1) => { if (g(e1, s ++ List(v))) true else false }
      case C(e1, e2) => { if (g(e1, s) && g(e2, s)) true else false }
    }
  }
    val check: Exp => Boolean = ( (exp) => { g(exp, Nil()) } )
}