import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub87 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup_x[A](x: A, env: List[A]): Boolean = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else lookup_x(x, tl) }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def func(lamd, env) = {
            lamd match {
              case V(x) => { lookup_x(x, env) }
              case P(x, l) => { func(l, x :: env) }
              case C(l1, l2) => { func(l1, env) && func(l2, env) }
            }
          }
          func(lam, Nil())
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