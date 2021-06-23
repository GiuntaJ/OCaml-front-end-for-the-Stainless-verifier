import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub82 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def inList(li, str) = {
            li match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == str) true else inList(tl, str) }
            }
          }
          val _5 = {
            def chk(l, ex) = {
              ex match {
                case V(x) => { inList(l, x) }
                case P(x, y) => { chk(x :: l, y) }
                case C(x, y) => { chk(l, x) && chk(l, y) }
              }
            }
            chk(Nil(), exp)
          }
        }
    }
  )
}