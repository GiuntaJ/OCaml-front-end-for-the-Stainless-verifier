import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub22 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  /* environment */
  
  val empty_env_0: List[A] = Nil()
  def extend_env_0[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_env_0[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_env_0(tl, x) }
    }
  }
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def check_0(lam, env) = {
            lam match {
              case V(x) => { apply_env_0(env, lam) }
              case P(x, f) => {
                val _7 = {
                  val proc = extend_env_0(V(x), env)
                  check_0(f, proc)
                }
              }
              case C(f1, f2) => {
                if (check_0(f1, env) == check_0(f2, env)) true else false
              }
            }
          }
          check_0(lam, empty_env_0)
        }
    }
  )
}