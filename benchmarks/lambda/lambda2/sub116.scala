import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub116 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def evalCheck1(ex: Exp, bd: Var): Boolean = {
    ex match {
      case V(x) => { false }
      case P(arg, body) => { if (arg == bd) true else evalCheck1(body, bd) }
      case C(a, b) => { evalCheck1(a, bd) || evalCheck1(b, bd) }
    }
  }
  def evalCheck2(ex: Exp, bd: Exp): Boolean = {
    bd match {
      case V(x) => { evalCheck1(ex, x) }
      case P(arg, body) => { evalCheck2(ex, body) }
      case C(a, b) => { evalCheck2(ex, a) && evalCheck2(ex, b) }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(x) => { false }
          case P(arg, body) => { evalCheck2(exp, body) }
          case C(a, b) => { check(a) && check(b) }
        }
    }
  )
}