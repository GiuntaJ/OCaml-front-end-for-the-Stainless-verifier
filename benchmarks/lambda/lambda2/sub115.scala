import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub115 {
  sealed case class NotImplemented() extends Exception {}
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def result: Exp => Var = (
    (exp) =>
      {
        exp match {
          case V(var0) => { var0 }
          case P(v, e) => { result(e) }
          case C(e1, e2) => { result(e2) }
        }
    }
  )
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(var0) => { false }
          case P(v1, V(v2)) => { if (v1 == v2) true else false }
          case P(v1, P(v2, e2)) => {
            if (check(P(v2, e2)) || v1 == result(e2)) true else false
          }
          case P(v1, C(e1, e2)) => { if (v1 == result(e2)) true else false }
          case _ => { assert(false, "NotImplemented") }
        }
    }
  )
}