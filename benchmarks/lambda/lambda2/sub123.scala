import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub123 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def chklst(((exp, lst))) = {
    exp match {
      case P(var0, exp) => { chklst(exp, lst ++ List(var0)) }
      case C(exp1, exp2) => {
        if (chklst(exp1, lst) && chklst(exp2, lst)) true else false
      }
      case V(var0) => {
        if (lst == Nil()) false else lst.exists(( (x) => { x == var0 } ))
      }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { chklst(exp, Nil()) } )
}