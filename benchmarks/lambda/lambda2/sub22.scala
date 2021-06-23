import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub22 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def find_exp(e, l) = {
    l match {
      case Cons(h, t) => { if (h == e) true else find_exp(e, t) }
      case Nil() => { false }
    }
  }
  
    val check: Exp => Boolean = (
    (e) =>
      {
        val _4 = {
          val var_lst = Nil()
          val _5 = {
            def check_el(ex, lst) = {
              ex match {
                case V(v) => { find_exp(v, lst) }
                case P(v, e1) => { check_el(e1, v :: lst) }
                case C(e1, e2) => { check_el(e1, lst) && check_el(e2, lst) }
              }
            }
            check_el(e, var_lst)
          }
        }
    }
  ) 
}