import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub101 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Env = List[(Var, Lambda)]
  
  def scan: (Env, Var) => Boolean = {
    case (env, var0) =>
      {
        env match {
          case Nil() => { false }
          case Cons(((v, l)), tl) => { if (v == var0) true else scan(tl, var0) }
        }
    }
  }
    
  def check_: (Env, Lambda) => Boolean = {
    case (env, lam) =>
      {
        lam match {
          case V(v) => { scan(env, v) }
          case P(v, l) => {
            val _2 = {
              val env = ((v, l)) :: env
              check_(env, l)
            }
          }
          case C(l1, l2) => { check_(env, l1) && check_(env, l2) }
        }
    }
  }
  
  val check: Lambda => Boolean = ( (lam) => { check_(Nil(), lam) } )
}