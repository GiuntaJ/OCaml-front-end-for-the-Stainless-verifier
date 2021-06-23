import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub128 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
   
    val listP = List("")
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(x) => { listP.contains(x) }
          case P(x, e1) => {
            val _2 = {
              val listP = listP ++ List(x)
              check(e1)
            }
          }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
}