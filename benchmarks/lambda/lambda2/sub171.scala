import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub171 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def chkList(((e, li))) = {
    e match {
      case V(var0) => {
        li match {
          case Nil() => { false }
          case Cons(h, t) => { if (var0 == h) true else chkList(V(var0), t) }
        }
      }
      case P(var0, exp) => { chkList(exp, var0 :: li) }
      case C(exp1, exp2) => {
        if (chkList(exp1, li) && chkList(exp2, li)) true else false
      }
    }
  }
    val check: Exp => Boolean = ( (exp) => { chkList(exp, Nil()) } )
    
}