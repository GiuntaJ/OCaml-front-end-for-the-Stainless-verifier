import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub83 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def var_find(exp: Exp): List[Var] = {
    exp match {
      case V(a) => { Nil() }
      case P(a, exp1) => { a :: var_find(exp1) }
      case C(exp1, exp2) => { var_find(exp1) ++ var_find(exp2) }
    }
  }
  
  def exp_find(exp: Exp): List[Var] = {
    exp match {
      case V(a) => { List(a) }
      case P(a, exp1) => { exp_find(exp1) }
      case C(exp1, exp2) => { exp_find(exp1) ++ exp_find(exp2) }
    }
  }
  
  def find[A](s: A, l: List[A]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (s == hd) true else find(s, tl) }
    }
  }
  
  def find_match[A](l1: List[A], l2: List[A]): Boolean = {
    l2 match {
      case Nil() => { true }
      case Cons(hd, tl) => { find(hd, l1) && find_match(l1, tl) }
    }
  }
  
   val check: Exp => Boolean = ( (exp) => { find_match(var_find(exp), exp_find(exp)) } )
}