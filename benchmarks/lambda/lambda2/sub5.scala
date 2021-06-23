import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub5 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def ch_list(((exp, lst))) = {
    exp match {
      case V(v) => { if (lst.contains(v)) true else false }
      case P(v, e) => { ch_list(e, lst ++ List(v)) }
      case C(e1, e2) => { ch_list(e1, lst) && ch_list(e2, lst) }
    }
  }
  
  val check: Exp => Boolean = ( (e) => { ch_list(e, Nil()) } )
}