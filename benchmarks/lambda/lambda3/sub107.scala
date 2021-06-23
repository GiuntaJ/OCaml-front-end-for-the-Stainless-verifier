import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub107 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def contains[A](l: List[A], x: A): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { if (h == x) true else contains(t, x) }
    }
  }
  
  def check_inner(bound_var: List[Var], prog: Lambda): Boolean = {
    prog match {
      case V(var0) => { if (contains(bound_var, var0)) true else false }
      case P(v, e) => { check_inner(v :: bound_var, e) }
      case C(e1, e2) => {
        check_inner(bound_var, e1) && check_inner(bound_var, e2)
      }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_inner(Nil(), lam) } )
  
  /* well-formed */
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  check(P("x", P("y", C(C(V("y"), V("y")), V("x")))))
  check(C(P("x", V("x")), P("y", V("y"))))
  check(P("x", P("y", C(V("x"), V("y")))))
  
  /* ill-formed */
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
  check(P("x", C(V("x"), P("z", C(V("x"), V("y"))))))
  check(C(V("x"), V("y")))
  check(V("x"))
  check(C(V("y"), P("x", C(V("x"), V("z")))))
}