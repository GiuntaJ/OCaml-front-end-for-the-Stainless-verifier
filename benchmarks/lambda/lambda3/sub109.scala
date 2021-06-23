import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub109 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  /* ----------------------------- my code start ------------------------------ */
  
  def findenv[A](env: List[A], x: A): Boolean = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else findenv(tl, x) }
    }
  }
  
  def lamchk(lam: Lambda, env: List[Var]): Boolean = {
    lam match {
      case V(x) => { findenv(env, x) }
      case P(px, pl) => { lamchk(pl, px :: env) }
      case C(cl1, cl2) => { lamchk(cl1, env) && lamchk(cl2, env) }
    }
  }
  
  /* ----------------------------- my code end------------------------------ */
  
  
  val check: Lambda => Boolean = ( (lam) => { lamchk(lam, Nil()) } )
  
  val pgm1: Lambda = P("a", V("a"))
  val pgm2: Lambda = P("a", P("a", V("a")))
  val pgm3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val pgm4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  
  val pgm5: Lambda = P("a", V("b"))
  val pgm6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val pgm7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  
  val test1: Lambda = C(P("x", P("y", V("x"))), V("y"))
  
  check(pgm1)
  check(pgm2)
  check(pgm3)
  check(pgm4)
  check(pgm5)
  check(pgm6)
  check(pgm7)
  
  check(test1)
}