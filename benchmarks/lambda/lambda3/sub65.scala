import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub65 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  type Var_env = List[String]
  
  def isbound: (Var_env, String) => Boolean = {
    case (env, str) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == str) true else isbound(tl, str) }
        }
    }
  }
  
  def check_lam(env, lam) = {
    lam match {
      case V(x) => { if (isbound(env, x)) true else false }
      case P(x, e) => {
        val _2 = {
          val nenv = x :: env
          check_lam(nenv, e)
        }
      }
      case C(e1, e2) => { check_lam(env, e1) && check_lam(env, e2) }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_lam(Nil(), lam) } )
}