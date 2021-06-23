import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub15 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
  val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          def check(x, y) = {
            x match {
              case V(x1) => { y.contains(x1) }
              case C(x1, y1) => { check(x1, y) && check(y1, y) }
              case P(x1, y1) => { check(y1, x1 :: y) }
            }
          }
          check(e, Nil())
        }
    }
  )
  
  
}
