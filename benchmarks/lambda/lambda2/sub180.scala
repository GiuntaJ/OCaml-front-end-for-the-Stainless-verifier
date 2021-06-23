import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub180 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  	def compare_var(a, b) = {
    b match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == a) true else compare_var(a, tl) }
    }
  }
  
  	def make_l(exp, l) = {
    exp match {
      case V(v) => { compare_var(v, l) }
      case P(v, e) => { make_l(e, l ++ List(v)) }
      case C(e1, e2) => { make_l(e1, l) && make_l(e2, l) }
    }
  }
  
    val check: Exp => Boolean = ( (exp) => { make_l(exp, Nil()) } )
}