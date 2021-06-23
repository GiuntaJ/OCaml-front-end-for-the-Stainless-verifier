import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub121 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def m(x, l) = {
            l match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == x) true else m(x, tl) }
            }
          }
          val _5 = {
            def find(ex, l) = {
              ex match {
                case P(x, y) => { find(y, x :: l) }
                case C(x, y) => { find(x, l) && find(y, l) }
                case V(x) => { m(x, l) }
              }
            }
            find(exp, Nil())
          }
        }
    }
  )
}