import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub21 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
    
    
    def lst_del(x, l) = {
    l match {
      case Nil() => { l }
      case Cons(h, t) => { if (h == x) lst_del(x, t) else h :: lst_del(x, t) }
    }
  }
    def _check(exp) = {
    exp match {
      case V(x) => { List(x) }
      case P(x, e) => { lst_del(x, _check(e)) }
      case C(e1, e2) => { _check(e1) ++ _check(e2) }
    }
  }
    
    val check: Exp => Boolean = ( (e) => { if (_check(e) == Nil()) true else false } )
}