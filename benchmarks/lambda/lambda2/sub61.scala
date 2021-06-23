import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub61 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  		 def checktest(a, b) = {
    a match {
      case V(x) => { b.contains(x) }
      case P(x, y) => { checktest(y, x :: b) }
      case C(x, y) => { checktest(x, b) && checktest(y, b) }
    }
  }
  		 def check(exp) = { checktest(exp, Nil()) }
}