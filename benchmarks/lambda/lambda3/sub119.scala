import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub119 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[String]
  
  def find_env: (List[A], A) => Boolean = {
    case (env, value) =>
      {
        env match {
          case Cons(v, tl) => { if (value == v) true else find_env(tl, value) }
          case Nil() => { false }
        }
    }
  }
  
  val add_env: (List[A], A) => List[A] = {
    case (env, value) => { env ++ List(value) }
  }
  
  def subcheck: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(v) => { find_env(env, v) }
          case P(v, y) => {
            val _2 = {
              val env_0 = add_env(env, v)
              subcheck(y, env_0)
            }
          }
          case C(l1, l2) => {
            if (subcheck(l1, env)) subcheck(l2, env) else false
          }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { subcheck(lam, Nil()) } )
  
        
        
        
        
        
        
        
}