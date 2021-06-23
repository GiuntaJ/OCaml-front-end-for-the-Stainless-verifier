import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub24 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def varArr(x, y_list) = {
            x match {
              case V(a) => { y_list.contains(a) }
              case P(a, b) => { varArr(b, a :: y_list) }
              case C(a, b) => { varArr(a, y_list) && varArr(b, y_list) }
            }
          }
          varArr(e, Nil())
        }
    }
  )
}