import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub41 {
  /*********************/
  /*    Problem2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[Var]
  val lambda_env: List[A] = Nil()
  def extend_str[A](x: A, e: List[A]): List[A] = { x :: e }
  def inplace_env[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else inplace_env(tl, x) }
    }
  }
  
  def check_lambda: (Lambda, Env) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(s) => { inplace_env(env, s) }
          case P(s, l) => {
            val _2 = {
              val env1 = extend_str(s, env)
              check_lambda(l, env1)
            }
          }
          case C(l1, l2) => { check_lambda(l1, env) && check_lambda(l2, env) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_lambda(lam, lambda_env) } )
}