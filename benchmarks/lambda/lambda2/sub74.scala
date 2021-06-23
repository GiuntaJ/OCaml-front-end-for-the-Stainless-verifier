import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub74 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
    def input_var: (Exp, List[String]) => List[String] = {
    case (e, lst) =>
      {
        e match {
          case V(v) => { lst }
          case P(v, e1) => { input_var(e1, lst) ++ List(v) }
          case C(e1, e2) => { input_var(e1, input_var(e2, lst)) }
        }
    }
  }
    def input_exp: (Exp, List[String]) => List[String] = {
    case (e, lst) =>
      {
        e match {
          case V(v) => { lst ++ List(v) }
          case P(v, e1) => { input_exp(e1, lst) }
          case C(e1, e2) => { input_exp(e1, input_exp(e2, lst)) }
        }
    }
  }
    def isthere: (List[String], List[String]) => Boolean = {
    case (lst, l2) =>
      {
        lst match {
          case Nil() => { true }
          case Cons(hd, tl) => {
            l2 match {
              case Nil() => { false }
              case Cons(h, t) => { if (hd == h) true else isthere(List(hd), t) }
            }
          }
        }
    }
  }
    def check: Exp => Boolean = (
    (exp) =>
      {
        val _4 = {
          val l2 = input_var(exp, Nil())
          val _5 = {
            val l1 = input_exp(exp, Nil())
            exp match {
              case V(v) => { false }
              case C(e1, e2) => { check(e1) && check(e2) }
              case P(v, e) => {
                l1 match {
                  case Nil() => { true }
                  case Cons(hd, tl) => {
                    if (isthere(List(hd), l2)) isthere(tl, l2) else false
                  }
                }
              }
            }
          }
        }
    }
  ) /* TODO */
}