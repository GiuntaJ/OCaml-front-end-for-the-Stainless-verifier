import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub43 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def remove(n, ls) = {
    ls match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { if (hd == n) remove(n, tl) else hd :: remove(n, tl)
      }
    }
  }
  
  def length(ls) = {
    ls match {
      case Nil() => { 0 }
      case Cons(hd, tl) => { 1 + length(tl) }
    }
  }
  
  def union(l1, l2) = {
    l1 match {
      case Nil() => { l2 }
      case Cons(hd, tl) => { union(tl, hd :: l2) }
    }
  }
  
  def setfree(lam) = {
    lam match {
      case V(x) => { List(x) }
      case P(x, lam1) => { remove(x, setfree(lam1)) }
      case C(lam1, lam2) => { union(setfree(lam1), setfree(lam2)) }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          val s = setfree(lam)
          if (length(s) == 0) true else false
        }
    }
  )
}