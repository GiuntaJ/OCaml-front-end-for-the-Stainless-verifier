import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub8 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  
  val bind: (List[Var], Var) => List[Var] = {
    case (env, v) => { v :: env }
  }
  
  val lookup: (List[Var], Var) => Boolean = {
    case (env, v) => { env.contains(v) }
  }
  
  
  def check_with_env: (Lambda, List[Var]) => Boolean = {
    case (lambda, env) =>
      {
        lambda match {
          case V(v) => { lookup(env, v) }
          case P(v, e) => {
            val _2 = {
              val new_env = bind(env, v)
              check_with_env(e, new_env)
            }
          }
          case C(e1, e2) => { check_with_env(e1, env) && check_with_env(e2, env)
          }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { check_with_env(lam, Nil()) } )
}
