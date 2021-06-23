import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub60 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def complst: (List[Var], Var) => Boolean = {
    case (lst, v) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == v) true else complst(tl, v) }
        }
    }
  }
  
  def checklst: (List[Var], Lambda) => Boolean = {
    case (lst, lam) =>
      {
        lam match {
          case V(x) => { complst(lst, x) }
          case P(x, l) => { checklst(x :: lst, l) }
          case C(l1, l2) => { checklst(lst, l1) && checklst(lst, l2) }
        }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(x) => { false }
          case P(x, l) => { checklst(List(x), l) }
          case C(l1, l2) => { check(l1) && check(l2) }
        }
    }
  )
}