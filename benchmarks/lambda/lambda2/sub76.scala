import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub76 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def evaluate(temp1, temp2) = {
    temp1 match {
      case V(a) => {
        val _2 = {
          def ck(l1, st) = {
            l1 match {
              case Nil() => { false }
              case Cons(h, t) => { if (h == st) true else ck(t, st) }
            }
          }
          if (ck(temp2, a)) true else false
        }
      }
      case P(v, e1) => { if (evaluate(e1, temp2 ++ List(v))) true else false }
      case C(e1, e2) => {
        if (evaluate(e1, temp2) && evaluate(e2, temp2)) true else false
      }
    }
  }
  
  val check: Exp => Boolean = ( (exp) => { evaluate(exp, Nil()) } )
}