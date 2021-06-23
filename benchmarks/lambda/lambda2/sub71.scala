import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub71 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  def check(input: Exp): Boolean = {
    val _2 = {
      def helpCheck(a, lst) = {
        a match {
          case V(a) => { lst.exists(( (a) => { a == a } )) }
          case P(a, b) => { helpCheck(b, a :: lst) }
          case C(a, b) => { helpCheck(a, lst) && helpCheck(b, lst) }
        }
      }
      helpCheck(input, Nil())
    }
  }
  
}
