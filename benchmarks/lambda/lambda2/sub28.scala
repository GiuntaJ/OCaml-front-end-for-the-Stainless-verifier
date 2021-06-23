import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub28 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case P(v, e) => {
            e match {
              case V(a) => { v == a }
              case P(a, b) => { check(e) }
              case C(a, b) => { check(a) && check(b) }
            }
          }
          case C(a, b) => { check(a) && check(b) }
          case V(a) => { true }
          case _ => { false }
        }
    }
  )
}