import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub58 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(x) => { false }
          case P(x, e1) => {
            e1 match {
              case V(y) => { x == y }
              case P(y, e2) => {
                e2 match {
                  case C(e3, e4) => {
                    (check(P(x, e3)) || check(P(x, e4))) &&
                    (check(P(y, e3)) || check(P(y, e4)))
                  }
                  case _ => { check(P(x, e2)) || check(P(y, e2)) }
                }
              }
              case C(e2, e3) => { check(P(x, e2)) && check(P(x, e3)) }
            }
          }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
}