import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub38 {
  	sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  	
  	val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def check_help(m, n_list) = {
            m match {
              case V(n) => { n_list.contains(n) }
              case P(n, e1) => { check_help(e1, n :: n_list) }
              case C(e1, e2) => {
                check_help(e1, n_list) && check_help(e2, n_list)
              }
            }
          }
          check_help(e, Nil())
        }
    }
  )
}