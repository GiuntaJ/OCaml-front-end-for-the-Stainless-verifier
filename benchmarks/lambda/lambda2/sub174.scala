import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub174 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(vari) => { true }
          case P(vari, exp) => {
            exp match {
              case V(v) => { if (vari == v) true else false }
              case C(e1, e2) => {
                (check(P(vari, e1)) || check(P(vari, e2))) && check(e1) &&
                check(e2)
              }
              case P(v, e) => { check(P(v, e)) && check(P(vari, e)) }
            }
          }
          case C(exp1, exp2) => { check(exp1) && check(exp2) }
        }
    }
  )
}