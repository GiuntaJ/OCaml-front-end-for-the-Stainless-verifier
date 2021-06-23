import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub40 {
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
  def extend_env[A](v: A, e: List[A]): List[A] = { v :: e }
  def apply_env[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_env(tl, x) }
    }
  }
  
  def isclosed: (Lambda, Env) => Boolean = {
    case (lam, curr_env) =>
      {
        lam match {
          case V(v) => { apply_env(curr_env, v) }
          case P(v, e) => { isclosed(e, extend_env(v, curr_env)) }
          case C(e1, e2) => {
            val _2 = {
              val b1 = isclosed(e1, curr_env)
              val _3 = {
                val b2 = isclosed(e2, curr_env)
                if (b1 && b2) true else false
              }
            }
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { isclosed(lam, empty_env) } )
}