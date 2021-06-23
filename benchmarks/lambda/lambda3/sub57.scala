import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub57 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def doesExist: (Var, List[A]) => Boolean = {
    case (var0, env) =>
      {
        env match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (var0 == hd) true else doesExist(var0, tl) }
        }
    }
  }
  
  def isBound: (Lambda, List[A]) => Boolean = {
    case (lam, env) =>
      {
        lam match {
          case V(v) => { if (doesExist(v, env)) true else false }
          case P(v, l) => { isBound(l, env ++ List(v)) }
          case C(l1, l2) => { isBound(l1, env) && isBound(l2, env) }
        }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(v) => { false }
          case P(v, l) => { isBound(l, List(v)) }
          case C(l1, l2) => { false }
        }
    }
  )
  
}
