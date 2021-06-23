import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub144 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    sealed abstract class Env {} case class Env(param0: List[Var]) extends Env {}
    def envHasIt(((env, var0))) = {
    env match {
      case Env(lst) => {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => {
            if (hd == var0) true else false || envHasIt(Env(tl), var0)
          }
        }
      }
    }
  }
    val addVarToEnv: (Env, Var) => Env = {
    case (env, var0) =>
      {
        env match {
          case Env(lst) => { Env(lst ++ List(var0)) }
        }
    }
  }
  
    def envCheck: (Env, Exp) => Boolean = {
    case (env, exp) =>
      {
        exp match {
          case V(v) => { envHasIt(env, v) }
          case P(v, e) => { envCheck(addVarToEnv(env, v), e) }
          case C(e1, e2) => { envCheck(env, e1) && envCheck(env, e2) }
        }
    }
  }
    val check: Exp => Boolean = ( (exp) => { envCheck(Env(Nil()), exp) } )
}