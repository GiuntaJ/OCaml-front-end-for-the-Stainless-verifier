import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub7 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def find[A](lst: List[A], var0: A): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == var0) true else find(tl, var0) }
    }
  }
  
  def compare(varlist: List[Var], lambda: Lambda): Boolean = {
    lambda match {
      case V(x) => { find(varlist, x) }
      case P(x, l) => { compare(varlist ++ List(x), l) }
      case C(l1, l2) => { compare(varlist, l1) && compare(varlist, l2) }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(x) => { false }
          case P(x, l) => { compare(List(x), l) }
          case C(l1, l2) => { check(l1) && check(l2) }
        }
    }
  )
  
}
