import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub63 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def eval(mlist: Exp): Boolean = {
    val _2 = {
      def ival(k, one_list) = {
        k match {
          case V(a) => { one_list.contains(a) }
          case P(a, b) => { ival(b, a :: one_list) }
          case C(a, b) => { ival(a, one_list) && ival(b, one_list) }
        }
      }
      ival(mlist, Nil())
    }
  }
  
  val check: Exp => Boolean = ( (e) => { if (eval(e) == true) true else false } )
}