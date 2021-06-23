import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub42 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  type Env = List[Var]
  
  
  def check_2: (Env, Lambda) => Boolean = {
    case (env, lamb) =>
      {
        lamb match {
          case V(v) => {
            env match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (v == hd) true else check_2(tl, lamb) }
            }
          }
          case P(v, lamb_0) => { check_2(List(v) ++ env, lamb_0) }
          case C(lamb_1, lamb_2) => {
            check_2(env, lamb_1) && check_2(env, lamb_2)
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_2(Nil(), lam) } )
  
  
}
