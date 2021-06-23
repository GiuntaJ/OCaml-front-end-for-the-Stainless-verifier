import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub159 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
      
  def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case P(a, b) => { true }
          case V(a) => { true }
          case C(a, b) => { true }
        }
    }
  )
}