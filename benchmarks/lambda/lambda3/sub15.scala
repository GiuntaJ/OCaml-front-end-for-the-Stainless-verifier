import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub15 {
    /*********************/
    /*   Problem 2        */
    /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def mklist(l, lam) = {
    lam match {
      case P(var0, lda) => { mklist(var0 :: l, lda) }
      case V(var0) => { l }
      case C(lda1, lda2) => {
        val _2 = {
          val l1 = mklist(l, lda1)
          val _3 = {
            val l2 = mklist(l, lda2)
            l1 ++ l2
          }
        }
      }
    }
  }
  def checker(l, var0) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (var0 == hd) true else checker(tl, var0) }
    }
  }
  
  def free_check(l, lam) = {
    lam match {
      case V(var0) => { checker(l, var0) }
      case P(var0, lda) => { free_check(l, lda) }
      case C(lda1, lda2) => {
        if (free_check(l, lda1) == true) free_check(l, lda2) else false
      }
    }
  }
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        lam match {
          case V(var0) => { false }
          case P(v, lam) => {
            val _10 = {
              val bound_list = mklist(Nil(), P(v, lam))
              free_check(bound_list, lam)
            }
          }
          case C(lda1, lda2) => {
            val _6 = {
              val b_list1 = mklist(Nil(), lda1)
              val _7 = {
                val b_list2 = mklist(Nil(), lda2)
                free_check(b_list1, lda1) && free_check(b_list2, lda2)
              }
            }
          }
        }
    }
  )
}