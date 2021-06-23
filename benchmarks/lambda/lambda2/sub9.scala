import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub9 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def checker(fv, bound_list) = {
            fv match {
              case V(a) => { bound_list.contains(a) }
              case P(a, b) => { checker(b, a :: bound_list) }
              case C(a, b) => { checker(a, bound_list) && checker(b, bound_list)
              }
            }
          }
          checker(e, Nil())
        }
    }
  )
}
