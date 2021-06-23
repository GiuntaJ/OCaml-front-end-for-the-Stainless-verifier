import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub73 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def extend[A](v: A, lis: List[A]): List[A] = { v :: lis }
  
  def find[A](v: A, lis: List[A]): Boolean = {
    lis match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else find(v, tl) }
    }
  }
  
  def checkfree: (Lambda, List[Var]) => Boolean = {
    case (lam, vlist) =>
      {
        lam match {
          case V(x) => { find(x, vlist) }
          case P(x, l) => { checkfree(l, x :: vlist) }
          case C(l1, l2) => { checkfree(l1, vlist) && checkfree(l2, vlist) }
        }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(x) => { false }
          case P(x, l) => { checkfree(l, List(x)) }
          case C(l1, l2) => { check(l1) && check(l2) }
        }
    }
  )
}