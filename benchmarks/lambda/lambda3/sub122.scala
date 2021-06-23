import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub122 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[Var]
  
  def find: (Var, Env) => Boolean = {
    case (x, env) =>
      {
        env match {
          case Cons(y, tl) => { if (x == y) true else find(x, tl) }
          case Nil() => { false }
        }
    }
  }
  
  def solve: (Lambda, Env) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { find(x, env) }
          case P(x, l) => { solve(l, x :: env) }
          case C(l1, l2) => { solve(l1, env) && solve(l2, env) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { solve(lam, Nil()) } )
}