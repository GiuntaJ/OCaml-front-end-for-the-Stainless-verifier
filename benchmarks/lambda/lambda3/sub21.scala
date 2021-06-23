import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub21 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[Var]
  
  /* environment */
  val empty_env: List[A] = Nil()
  def extend_env[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_env[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (x == hd) true else apply_env(tl, x) }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def func(lam, env) = {
            lam match {
              case V(a) => { if (apply_env(env, a) == true) true else false }
              case P(a, l) => { func(l, extend_env(a, env)) }
              case C(l1, l2) => { func(l1, env) && func(l2, env) }
            }
          }
          func(lam, empty_env)
        }
    }
  ) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
