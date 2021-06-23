import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub26 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def extend(v, env) = { v :: env }
          val _5 = {
            def find(v, env) = { env.contains(v) }
            val _6 = {
              def check_env(lambda, env) = {
                lambda match {
                  case V(v) => { find(v, env) }
                  case P(v, l) => {
                    val _9 = {
                      val newenv = extend(v, env)
                      check_env(l, newenv)
                    }
                  }
                  case C(l1, l2) => { check_env(l1, env) && check_env(l2, env) }
                }
              }
              check_env(lam, Nil())
            }
          }
        }
    }
  )
}