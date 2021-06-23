import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub27 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def chars: Exp => Var = (
    (e) =>
      {
        e match {
          case V(a) => { a }
          case P(v, e1) => {
            e1 match {
              case V(b) => { b }
              case P(b, e2) => { chars(e2) }
              case C(e1, e2) => { chars(e2) }
            }
          }
          case C(e1, e2) => {
            (e1, e2) match {
              case (V(b), V(c)) => { c }
              case (V(b), P(c, e3)) => { chars(e3) }
            }
          }
        }
    }
  )
  
  
  val check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(a) => { true }
          case P(v, e1) => {
            e1 match {
              case V(b) => { if (v == b) true else false }
              case P(b, e2) => {
                if (chars(e2) == v || chars(e2) == b) true else false
              }
              case C(e1, e2) => {
                if (chars(e1) == v || chars(e2) == v) true else false
              }
            }
          }
          case C(e1, e2) => {
            (e1, e2) match {
              case (V(b), V(c)) => { true }
              case (V(b), P(c, e3)) => {
                if (c == b || b == chars(e3)) true else false
              }
            }
          }
        }
    }
  )
}
