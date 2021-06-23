import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub61 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val env = Nil()
          val _5 = {
            def extend_env(v, env) = { v :: env }
            val _6 = {
              def find_env(v, env) = {
                env match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (hd == v) true else find_env(v, tl)
                  }
                }
              }
              val _7 = {
                def eval(lam, env) = {
                  lam match {
                    case V(v) => { find_env(v, env) }
                    case P(v, l) => {
                      val _10 = {
                        val env_0 = extend_env(v, env)
                        eval(l, env_0)
                      }
                    }
                    case C(l1, l2) => { eval(l1, env) && eval(l2, env) }
                  }
                }
                eval(lam, env)
              }
            }
          }
        }
    }
  )
}