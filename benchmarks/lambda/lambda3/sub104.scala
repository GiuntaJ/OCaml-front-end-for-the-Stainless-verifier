import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub104 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def in_env[A](env: List[A], var0: A): Boolean = {
    env match {
      case Cons(hd, tl) => { if (hd == var0) true else in_env(tl, var0) }
      case Nil() => { false }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def free(env, exp) = {
            exp match {
              case V(v) => { in_env(env, v) }
              case P(v, l) => { free(v :: env, l) }
              case C(l1, l2) => { free(env, l1) && free(env, l2) }
            }
          }
          free(Nil(), lam)
        }
    }
  )
      
}