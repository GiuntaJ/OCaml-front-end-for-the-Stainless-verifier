import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub24 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
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
          case V(str1) => { apply_env(env, str1) }
          case P(str1, lam1) => { check_env(lam1, extend_env(str1, env)) }
          case C(lam1, lam2) => { check_env(lam1, env) && check_env(lam2, env) }
        }
    }
  }
  
  
  def check: Lambda => Boolean = ( (lam) => { check_env(lam, empty_env) } )
}