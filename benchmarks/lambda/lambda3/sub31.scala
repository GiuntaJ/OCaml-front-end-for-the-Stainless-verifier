import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub31 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def extenv[A](x: A, env: List[A]): List[A] = { x :: env }
  def search[A](env: List[A], lam: A): Boolean = {
    env match {
      case Nil() => { false }
      case Cons(a, tl) => { if (lam == a) true else search(tl, lam) }
    }
  }
  
  def checklambda(lambda, env) = {
    lambda match {
      case V(x) => { search(env, x) }
      case P(x, l) => {
        val _2 = {
          val newenv1 = extenv(x, env)
          checklambda(l, newenv1)
        }
      }
      case C(l1, l2) => { checklambda(l1, env) && checklambda(l2, env) }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { checklambda(lam, Nil()) } )
}