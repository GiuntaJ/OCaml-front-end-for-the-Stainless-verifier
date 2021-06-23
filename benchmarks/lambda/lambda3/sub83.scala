import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub83 {
  /*********************/
  /*   Problem 2        */
  /********************/
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  type Env = List[Var]
  
  def extend_env[A](x: A, e: List[A]): List[A] = { x :: e }
  def apply_env[A](e: List[A], x: A): Boolean = {
    e match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else apply_env(tl, x) }
    }
  }
  
  def eval: (Lambda, Env) => Boolean = {
    case (l, e) =>
      {
        l match {
          case V(x) => { if (apply_env(e, x) == true) true else false }
          case P(x, l1) => { eval(l1, extend_env(x, e)) }
          case C(l1, l2) => { eval(l1, e) && eval(l2, e) }
        }
    }
  }
  
  def check: Lambda => Boolean = ( (lam) => { eval(lam, Nil()) } )
}
