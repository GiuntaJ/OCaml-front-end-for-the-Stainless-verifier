import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub51 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def varfind(arr, x) = {
    arr match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == x) true else varfind(tl, x) }
    }
  }
  
  def find(arr, lam) = {
    lam match {
      case V(x) => { (varfind(arr, x), arr) }
      case P(x, l) => {
        val _6 = {
          val arr_0 = x :: arr
          find(arr_0, l)
        }
      }
      case C(l1, l2) => {
        val _2 = {
          val ((b, arr_0)) = find(arr, l1)
          val _3 = {
            val ((b2, arr__0)) = find(arr_0, l2)
            (b && b2, arr__0)
          }
        }
      }
    }
  }
    
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _11 = {
          val ((b, a)) = find(Nil(), lam)
          b
        }
    }
  )
}