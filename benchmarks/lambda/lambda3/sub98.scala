import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub98 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[Var]
  
  val ext_env: (Var, Env) => Env = {
    case (v, e) => { v :: e }
  }
  
  def get_env: (Var, Env) => Boolean = {
    case (v, e) =>
      {
        e match {
          case Cons(hd, tl) => { if (hd == v) true else get_env(v, tl) }
          case Nil() => { false }
        }
    }
  }
  
  def check_rec: (Lambda, Env) => Boolean = {
    case (lam, e) =>
      {
        lam match {
          case V(v) => { if (get_env(v, e)) true else false }
          case P(v, l) => { check_rec(l, ext_env(v, e)) }
          case C(l1, l2) => { check_rec(l1, e) && check_rec(l2, e) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_rec(lam, Nil()) } )
  
  val lam1: Lambda = P("a", V("a"))
  val lam2: Lambda = P("a", P("a", V("a")))
  val lam3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val lam4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  val lam5: Lambda = P("a", V("b"))
  val lam6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val lam7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  check(lam1)
  check(lam2)
  check(lam3)
  check(lam4)
  check(lam5)
  check(lam6)
  check(lam7)
}