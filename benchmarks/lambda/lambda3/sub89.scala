import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub89 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  def extend_env[A](x: A, env: List[A]): List[A] = { x :: env }
  
  def is_in(lam: Lambda, env: List[Var]): Boolean = {
    lam match {
      case V(x) => {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (x == hd) true else is_in(lam, tl) }
        }
      }
      case P(x, lam1) => { is_in(lam1, x :: env) }
      case C(lam1, lam2) => { is_in(lam1, env) && is_in(lam2, env) }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { is_in(lam, Nil()) } )
  
  val pgm1: Lambda = P("a", V("a"))
  val pgm2: Lambda = P("a", P("a", V("a")))
  val pgm3: Lambda = P("a", P("b", C(V("a"), V("b"))))
  val pgm4: Lambda = P("a", C(V("a"), P("b", V("a"))))
  val pgm5: Lambda = P("a", V("b"))
  val pgm6: Lambda = P("a", C(V("a"), P("b", V("c"))))
  val pgm7: Lambda = P("a", P("b", C(V("a"), V("c"))))
  check(pgm1)
  check(pgm2)
  check(pgm3)
  check(pgm4)
  check(pgm5)
  check(pgm6)
  check(pgm7)
}