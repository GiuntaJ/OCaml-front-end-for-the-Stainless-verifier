import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub32 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env2 = List[Var]
  val empty_env2: List[A] = Nil()
  def extend_env2[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_env2[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(y, tl) => { if (x == y) true else apply_env2(tl, x) }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def help2: (Lambda, Env2) => Boolean = {
            case (lam, env) =>
              {
                lam match {
                  case V(x) => { apply_env2(env, x) }
                  case P(x, lam2) => {
                    val _7 = {
                      val env2 = extend_env2(x, env)
                      help2(lam2, env2)
                    }
                  }
                  case C(lam1, lam2) => {
                    help2(lam1, env) match {
                      case true => { help2(lam2, env) }
                      case false => { false }
                    }
                  }
                }
            }
          }
          help2(lam, empty_env2)
        }
    }
  )
}