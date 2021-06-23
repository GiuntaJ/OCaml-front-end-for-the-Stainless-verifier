import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub139 {
  /*  Problem4  */
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  val empty_env: List[A] = Nil()
  def extend_env[A](x: A, e: List[A]): List[A] = { x :: e }
  def lookup_env[A](x: A, e: List[A]): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (x == hd) true else lookup_env(x, tl) }
    }
  }
  
  def check(f: Exp): Boolean = {
    val _2 = {
      def _check(fu, en) = {
        fu match {
          case P(x, y) => {
            val _5 = {
              val env_0 = extend_env(x, en)
              _check(y, env_0)
            }
          }
          case V(x) => { if (lookup_env(x, en)) true else false }
          case C(x, y) => { if (_check(x, en) == true) _check(y, en) else false
          }
        }
      }
      _check(f, empty_env)
    }
  }
}