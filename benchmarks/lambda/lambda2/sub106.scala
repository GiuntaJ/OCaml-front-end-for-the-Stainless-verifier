import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub106 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def check3(v, l) = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == v) true else check3(v, tl) }
    }
  }
  
    def check2(e, l) = {
    e match {
      case V(v) => { check3(v, l) }
      case P(v, exp1) => { check2(exp1, l ++ List(v)) }
      case C(exp2, exp3) => { check2(exp2, l) && check2(exp3, l) }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { check2(exp, Nil()) } )
}