import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub97 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val empty_env = Nil()
  
    def lookup_env(env, var0) = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == var0) true else lookup_env(tl, var0) }
    }
  }
  
  
    def extend_env(env, v) = { v :: env }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def c_env(env, ex) = {
            ex match {
              case V(a) => { lookup_env(env, a) }
              case P(a, b) => {
                val _7 = {
                  val env2 = extend_env(env, a)
                  c_env(env2, b)
                }
              }
              case C(a, b) => {
                if (c_env(env, a) && c_env(env, b)) true else false
              }
            }
          }
          c_env(empty_env, exp)
        }
    }
  )
}