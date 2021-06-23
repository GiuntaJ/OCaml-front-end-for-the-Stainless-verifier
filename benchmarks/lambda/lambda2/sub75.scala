import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub75 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { true }
          case P(v, e) => { true }
          case C(e1, e2) => { true }
        }
    }
  )
}