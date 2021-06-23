import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub17 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type Bounded = List[Var]
  
  def update_bounded[A](a: A, stk: List[A]): List[A] = { a :: stk }
  def find_bounded[A](a: A, stk: List[A]): Boolean = {
    stk match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == a) true else find_bounded(a, tl) }
    }
  }
  
  def boundCheck: (Lambda, Bounded) => Boolean = {
    case (lam, bd) =>
      {
        lam match {
          case V(v) => { find_bounded(v, bd) }
          case P(v, l) => {
            val _2 = {
              val s = update_bounded(v, bd)
              boundCheck(l, s)
            }
          }
          case C(l1, l2) => {
            if (boundCheck(l2, bd)) boundCheck(l2, bd) else false
          }
        }
    }
  }
  
  
  def check: Lambda => Boolean = ( (lam) => { boundCheck(lam, Nil()) } )
  
  
  
}
