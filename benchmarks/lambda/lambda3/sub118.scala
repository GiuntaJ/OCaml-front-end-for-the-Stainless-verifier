import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub118 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def find(v, lst) = {
            lst match {
              case Cons(x, xs) => { if (x == v) true else find(v, xs) }
              case Nil() => { false }
            }
          }
          val _5 = {
            def check_0(vlst, expr) = {
              expr match {
                case V(v) => { find(v, vlst) }
                case P(v, b) => { check_0(v :: vlst, b) }
                case C(f, a) => { check_0(vlst, f) && check_0(vlst, a) }
              }
            }
            check_0(Nil(), lam)
          }
        }
    }
  )
  
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
}