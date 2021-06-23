import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub20 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env1 = List[(Var, Boolean)]
  
  val empty_env: List[A] = Nil()
  def extender_env(((x, v)), e) = { ((x, v)) :: e }
  def applyer_env[A](e: List[(A, Boolean)], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(((y, v)), tl) => { if (x == y) v else applyer_env(tl, x) }
    }
  }
  
  def custom_check: (Lambda, Env1) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { applyer_env(env, x) }
          case P(x, l) => { custom_check(l, extender_env(x, true, env)) }
          case C(l1, l2) => {
            if (custom_check(l1, env)) custom_check(l2, env) else false
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { custom_check(lam, empty_env) } )
}