import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub64 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check: Exp => Boolean = (
    (e) =>
      {
        e match {
          case V(x) => { true }
          case P(x, y) => { check(y) && checkx(x, y) }
          case C(x, y) => { check(y) && check(x) }
        }
    }
  )
  def checkx(((a, b))) = {
    b match {
      case V(x) => { if (x == a) true else false }
      case P(x, y) => { checkx(a, y) }
      case C(x, y) => { checkx(a, x) || checkx(a, y) }
    }
  }
}