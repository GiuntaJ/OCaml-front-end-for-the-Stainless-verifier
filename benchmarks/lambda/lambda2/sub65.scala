import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub65 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def remove_var(exp, var0) = {
            exp match {
              case P(v, ex) => { P(v, remove_var(ex, var0)) }
              case C(ex, ex_0) => {
                C(remove_var(ex, var0), remove_var(ex_0, var0))
              }
              case V(v) => { if (v == var0) V("") else V(v) }
            }
          }
          e match {
            case P(v, ex) => { check(remove_var(ex, v)) }
            case C(ex, ex_0) => { check(ex) && check(ex_0) }
            case V(v) => { if (v != "") false else true }
          }
        }
    }
  )
}