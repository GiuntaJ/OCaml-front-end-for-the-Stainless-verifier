import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub6 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  def recheck(v, lamm) = {
    lamm match {
      case P(x, l) => { recheck(v ++ List(x), l) }
      case C(l1, l2) => { recheck(v, l1) && recheck(v, l2) }
      case V(x) => {
        v match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == x) true else recheck(tl, lamm) }
        }
      }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case P(v, l) => { recheck(List(v), l) }
          case _ => { false }
        }
    }
  )
}