import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub130 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def set_var: (Exp, List[Var]) => Boolean = {
    case (exp, str) =>
      {
        exp match {
          case V(r) => { if (str.contains(r)) true else false }
          case P(aexp, e) => { set_var(e, List(aexp) ++ str) }
          case C(e1, e2) => { set_var(e1, str) && set_var(e2, str) }
        }
    }
  }
  
    def check: Exp => Boolean = (
    (exp) =>
      {
        exp match {
          case V(str) => { false }
          case P(str, e) => { set_var(e, List(str)) }
          case C(e1, e2) => { check(e1) && check(e2) }
        }
    }
  ) /* TODO */
}