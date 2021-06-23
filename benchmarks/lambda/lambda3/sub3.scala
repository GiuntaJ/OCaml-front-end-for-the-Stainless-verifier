import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub3 {
  
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
          def find(v, lst) = {
            lst match {
              case Cons(hd, tl) => { if (hd == v) true else find(v, tl) }
              case Nil() => { false }
            }
          }
          val _5 = {
            def func(lam, lst) = {
              lam match {
                case V(v) => { if (find(v, lst)) true else false }
                case P(v, l) => { func(l, v :: lst) }
                case C(l1, l2) => { func(l1, lst) && func(l2, lst) }
              }
            }
            func(lam, Nil())
          }
        }
    }
  )
}