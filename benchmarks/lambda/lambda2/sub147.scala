import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub147 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def contains(v, vlist) = {
            vlist match {
              case Cons(h, t) => { if (v == h) true else contains(v, t) }
              case _ => { false }
            }
          }
          val _5 = {
            def chk(e, vlist) = {
              e match {
                case P(v, ne) => { chk(ne, v :: vlist) }
                case C(e1, e2) => { chk(e1, vlist) && chk(e2, vlist) }
                case V(v) => { contains(v, vlist) }
              }
            }
            chk(exp, Nil())
          }
        }
    }
  )
}