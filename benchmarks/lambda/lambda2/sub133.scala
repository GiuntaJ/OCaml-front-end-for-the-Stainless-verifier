import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub133 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(_) => { false }
          case C(_, _) => { false }
          case P(v1, V(v2)) => { if (v1 == v2) true else false }
          case P(v1, P(v2, e1)) => {
            if (v1 == v2 && check(P(v1, e1))) true else false
          }
          case P(v1, C(e1, e2)) => { check(P(v1, e1)) && check(P(v1, e2)) }
        }
    }
  )
}
