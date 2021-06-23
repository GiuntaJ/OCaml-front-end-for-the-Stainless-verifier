import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub177 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(a) => { false }
          case P(a, b) => {
            exp match {
              case V(c) => { if (a == c) true else false }
              case P(c, d) => { check(P(c, d)) || check(P(a, d)) }
              case C(c, d) => { check(P(a, c)) && check(P(a, d)) }
            }
          }
          case C(a, b) => {
            (a, b) match {
              case (P(c, d), P(e, f)) => { check(P(c, d)) && check(P(e, f)) }
              case _ => { false }
            }
          }
        }
    }
  )
}