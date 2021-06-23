import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub25 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find[A](x: A, arr: List[A]): Boolean = {
    arr match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else find(x, tl) }
    }
  }
  
  
  def apply(lam: Lambda, var0: List[Var]): Boolean = {
    lam match {
      case V(x) => { find(x, var0) }
      case P(x, l) => { apply(l, x :: var0) }
      case C(l1, l2) => { apply(l1, var0) && apply(l2, var0) }
    }
  }
  
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(x) => { false }
          case P(x, l) => { apply(l, List(x)) }
          case C(l1, l2) => { check(l1) && check(l2) }
        }
    }
  )
}