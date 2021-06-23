import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub13 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def append_item(lst, a) = { lst ++ List(a) }
   
    def equiv(lst, a) = {
    lst match {
      case Nil() => { false }
      case Cons(last, Nil()) => { if (a == last) true else false }
      case Cons(hd, tl) => { if (a == hd) true else equiv(tl, a) }
    }
  }
  
    def check_scope(e, lst) = {
    e match {
      case V(a) => { equiv(lst, a) }
      case P(a, e) => { check_scope(e, append_item(lst, a)) }
      case C(e1, e2) => { check_scope(e1, lst) && check_scope(e2, lst) }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          val init = Nil()
          check_scope(e, init)
        }
    }
  )
}