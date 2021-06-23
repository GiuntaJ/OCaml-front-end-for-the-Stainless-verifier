import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub119 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    type Env = List[Var]
  
    val empty_env = Nil()
  
    def lookup_env(env, v) = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else lookup_env(tl, v) }
    }
  }
  
    def extend_env(env, var0) = { var0 :: env }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def check_env(env, e) = {
            e match {
              case V(x) => { lookup_env(env, x) }
              case P(x, y) => {
                val _7 = {
                  val env_0 = extend_env(env, x)
                  check_env(env_0, y)
                }
              }
              case C(x, y) => {
                if (check_env(env, x) && check_env(env, y)) true else false
              }
            }
          }
          check_env(empty_env, exp)
        }
    }
  )
}