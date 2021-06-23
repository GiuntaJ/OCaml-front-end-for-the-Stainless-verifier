import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub76 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  /* environment */
  val empty_lam: List[A] = Nil()
  def extend_lam[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_lam[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_lam(tl, x) }
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
          def lamb(lam, env) = {
            lam match {
              case V(va) => { apply_lam(env, lam) }
              case P(va, la1) => {
                val _7 = {
                  val env_0 = extend_lam(V(va), env)
                  lamb(la1, env_0)
                }
              }
              case C(la1, la2) => {
                if (lamb(la1, env) && lamb(la2, env)) true else false
              }
            }
          }
          lamb(lam, empty_lam)
        }
    }
  )
}