import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub156 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def is_V_here: (List[String], String) => Boolean = {
    case (lst, var0) =>
      {
        lst match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else is_V_here(tl, var0) }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(v) => { false }
          case P(v, e) => { sub_check(List(v), e) }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  )
  def sub_check: (List[String], Exp) => Boolean = {
    case (lst, exp) =>
      {
        exp match {
          case V(v) => { is_V_here(lst, v) }
          case P(v, e) => { sub_check(v :: lst, e) }
          case C(e1, e2) => { sub_check(lst, e1) && sub_check(lst, e2) }
        }
    }
  }
}