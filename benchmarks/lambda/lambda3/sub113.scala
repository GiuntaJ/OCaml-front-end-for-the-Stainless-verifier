import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub113 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find_env: (Var, List[Var]) => Boolean = {
    case (var0, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else find_env(var0, tl) }
        }
    }
  }
  
  def lambda_env: (Lambda, List[Var]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(x) => { if (find_env(x, env) == false) false else true }
          case P(x, l1) => { lambda_env(l1, x :: env) }
          case C(l1, l2) => {
            val _2 = {
              val t1 = lambda_env(l1, env)
              val _3 = {
                val t2 = lambda_env(l2, env)
                if (t1 == true && t2 == true) true else false
              }
            }
          }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { lambda_env(lam, Nil()) } )
}