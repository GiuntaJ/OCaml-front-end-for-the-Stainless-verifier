import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub111 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def find(v, lst) = {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == v) true else find(v, tl) }
            }
          }
          val _5 = {
            def ch(la, lst) = {
              la match {
                case V(v) => { find(v, lst) }
                case P(v, l) => { ch(l, v :: lst) }
                case C(l1, l2) => { ch(l1, lst) && ch(l2, lst) }
              }
            }
            ch(lam, Nil())
          }
        }
    }
  )
}