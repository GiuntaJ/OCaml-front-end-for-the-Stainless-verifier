import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub23 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  val empty_env2: List[A] = Nil()
  def extend_env2[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_env2[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_env2(tl, x) }
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
          def help_check(lam, env) = {
            lam match {
              case V(x) => { if (apply_env2(env, x) eq true) true else false }
              case P(x, y) => { help_check(y, extend_env2(x, env)) }
              case C(x, y) => { help_check(x, env) && help_check(y, env) }
            }
          }
          help_check(lam, empty_env2)
        }
    }
  )
}