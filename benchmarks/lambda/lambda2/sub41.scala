import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub41 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    def check_list(((exp, lst))) = {
    exp match {
      case V(var0) => { if (lst.contains(var0)) true else false }
      case P(var0, exp) => { check_list(exp, lst ++ List(var0)) }
      case C(exp1, exp2) => { check_list(exp2, lst) && check_list(exp2, lst) }
    }
  }
  
    val check: Exp => Boolean = ( (e) => { check_list(e, Nil()) } )
}