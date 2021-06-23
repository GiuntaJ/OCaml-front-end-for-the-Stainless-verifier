import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub172 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          def finalcheck(e, l) = {
            l match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd eq e) true else finalcheck(e, tl) }
            }
          }
          val _5 = {
            def checklst(e, l) = {
              e match {
                case P(v, e) => { checklst(e, l ++ List(v)) }
                case C(e1, e2) => { checklst(e1, l) && checklst(e2, l) }
                case V(v) => { finalcheck(v, l) }
              }
            }
            checklst(exp, Nil())
          }
        }
    }
  )
}