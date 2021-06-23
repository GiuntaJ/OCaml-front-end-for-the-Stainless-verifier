import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub39 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def helpCheck(exp, var_list) = {
            exp match {
              case V(var0) => { var_list.contains(var0) }
              case P(var0, e1) => { helpCheck(e1, var0 :: var_list) }
              case C(e1, e2) => {
                helpCheck(e1, var_list) && helpCheck(e2, var_list)
              }
            }
          }
          helpCheck(e, Nil())
        }
    }
  )
}