import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub96 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def append_list(list1, list2) = {
    list1 match {
      case Nil() => { list2 }
      case Cons(hd, tl) => { hd :: append_list(tl, list2) }
    }
  }
  
    def get_variables(exp, result) = {
    exp match {
      case V(v) => { v :: result }
      case P(v, e) => { get_variables(e, v :: result) }
      case C(e1, e2) => {
        append_list(get_variables(e1, result), get_variables(e2, result))
      }
    }
  }
  
    def var_in_exp(variable, exp) = {
    exp match {
      case V(v) => { false }
      case P(v, e) => { if (v == variable) true else var_in_exp(variable, e) }
      case C(e1, e2) => { var_in_exp(variable, e1) || var_in_exp(variable, e2) }
    }
  }
  
    def check_helper(variables, exp) = {
    variables match {
      case Nil() => { true }
      case Cons(hd, tl) => {
        if (var_in_exp(hd, exp) == false) false else check_helper(tl, exp)
      }
    }
  }
  
    val check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val variables = get_variables(exp, Nil())
          check_helper(variables, exp)
        }
    }
  )
}