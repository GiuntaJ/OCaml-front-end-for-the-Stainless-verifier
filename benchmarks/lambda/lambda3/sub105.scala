import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub105 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup_env: (Var, List[Var]) => Boolean = {
    case (x, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(y, tl) => { if (x == y) true else lookup_env(x, tl) }
        }
    }
  }
  
  def extend_env: (Var, List[Var]) => List[Var] = {
    case (x, env) => { x :: env }
  }
  
  def any_free: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { lookup_env(x, env) }
          case P(x, l) => {
            val _2 = {
              val new_env = extend_env(x, env)
              any_free(l, new_env)
            }
          }
          case C(l1, l2) => { any_free(l1, env) && any_free(l2, env) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { any_free(lam, Nil()) } )
}