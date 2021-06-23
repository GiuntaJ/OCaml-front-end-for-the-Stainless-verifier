import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub99 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup: (List[Var], Var) => Boolean = {
    case (l, v) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else lookup(tl, v) }
        }
    }
  }
  
  val ex_env: (List[Var], Var) => List[Var] = {
    case (l, v) => { v :: l }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val env = Nil()
          val _5 = {
            def ch(lam, env) = {
              lam match {
                case V(v) => { lookup(env, v) }
                case P(v, l) => { ch(l, ex_env(env, v)) }
                case C(l1, l2) => { ch(l1, env) && ch(l2, env) }
              }
            }
            ch(lam, env)
          }
        }
    }
  )
}