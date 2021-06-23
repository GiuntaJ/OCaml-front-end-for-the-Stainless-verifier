import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub68 {
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
          def find(x, l) = {
            l match {
              case Nil() => { false }
              case Cons(h, t) => { if (x == h) true else find(x, t) }
            }
          }
          val _5 = {
            def check2(la, l) = {
              la match {
                case V(v) => { find(v, l) }
                case P(v, la1) => { check2(la1, v :: l) }
                case C(la1, la2) => { check2(la1, l) && check2(la2, l) }
              }
            }
            check2(lam, Nil())
          }
        }
    }
  )
}