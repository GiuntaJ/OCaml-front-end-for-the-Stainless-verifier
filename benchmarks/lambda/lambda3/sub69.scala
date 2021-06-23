import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub69 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def findvar(l, v) = {
            l match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == v) true else findvar(tl, v) }
            }
          }
          val _5 = {
            def impl(l, e) = {
              e match {
                case V(v) => { findvar(l, v) }
                case P(v, e_) => { impl(v :: l, e_) }
                case C(e1, e2) => { impl(l, e1) eq true && impl(l, e2) eq true }
              }
            }
            impl(Nil(), lam)
          }
        }
    }
  )
}