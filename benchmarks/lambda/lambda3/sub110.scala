import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub110 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = ( (lam) => { test(lam, Nil()) } )
  def test: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(v) => { find_env(v, env) }
          case P(v, l) => { test(l, v :: env) }
          case C(l1, l2) => { test(l1, env) && test(l2, env) }
        }
    }
  }
  def find_env: (Var, List[Var]) => Boolean = {
    case (v, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == v) true else find_env(v, t) }
        }
    }
  }
}
