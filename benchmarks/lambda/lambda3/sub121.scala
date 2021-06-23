import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub121 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup[A](var0: A, env: List[A]): Boolean = {
    env match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == var0) true else lookup(var0, tl) }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def alpaca(env, lam) = {
            lam match {
              case V(x) => { lookup(x, env) }
              case P(x, lam1) => {
                val _7 = {
                  val newenv = x :: env
                  alpaca(newenv, lam1)
                }
              }
              case C(lam1, lam2) => { alpaca(env, lam1) && alpaca(env, lam2) }
            }
          }
          alpaca(Nil(), lam)
        }
    }
  )
  
  
  
}
