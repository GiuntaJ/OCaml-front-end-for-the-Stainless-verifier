import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub132 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[Var]
  
  val empty_env: List[A] = Nil()
  def extend_env[A](x: A, lst: List[A]): List[A] = { x :: lst }
  def apply_env[A](lst: List[A], x: A): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_env(tl, x) }
    }
  }
    
  def check_env: (Lambda, Env) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(var0) => { apply_env(env, var0) }
          case P(var0, lam) => { check_env(lam, extend_env(var0, env)) }
          case C(lam1, lam2) => { check_env(lam1, env) && check_env(lam2, env) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_env(lam, empty_env) } )
  
  
  check(P("a", P("b", C(V("a"), V("b"))))) /*true*/
  
  check(P("a", C(V("a"), P("b", V("a"))))) /*true*/
  
  check(P("a", C(V("a"), P("b", V("c"))))) /*false*/
  check(P("a", P("b", C(V("a"), V("c"))))) /*false*/ 
}