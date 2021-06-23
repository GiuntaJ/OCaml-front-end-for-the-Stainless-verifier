import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sol {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def is_mem: (List[Var], Var) => Boolean = {
    case (variables, var0) =>
      {
        variables match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == var0) true else is_mem(tl, var0) }
        }
    }
  }
  
  def sub_check: (Exp, List[Var]) => Boolean = {
    case (exp, vars) =>
      {
        exp match {
          case V(x) => { is_mem(vars, x) }
          case P(x, e) => { sub_check(e, x :: vars) }
          case C(e1, e2) => { sub_check(e1, vars) && sub_check(e2, vars) }
        }
    }
  }
  
  def check: Exp => Boolean = ( (exp) => { sub_check(exp, Nil()) } )
}