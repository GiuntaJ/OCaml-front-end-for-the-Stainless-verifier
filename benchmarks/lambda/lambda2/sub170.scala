import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub170 {
  
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def str(exp) = {
    exp match {
      case V(a) => { Nil() }
      case P(a, e1) => { a :: str(e1) }
      case C(e1, e2) => { str(e1) ++ str(e2) }
    }
  }
  
  def v_str(exp) = {
    exp match {
      case V(a) => { List(a) }
      case P(a, e1) => { v_str(e1) }
      case C(e1, e2) => { v_str(e1) ++ v_str(e2) }
    }
  }
  
  def search(var0, exp) = {
    exp match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == var0) true else search(var0, tl) }
    }
  }
  
  def compare(e1, e2) = {
    e2 match {
      case Nil() => { true }
      case Cons(hd, tl) => {
        if (search(hd, e1) && compare(e1, tl)) true else false
      }
    }
  }
  
  
  
  
  val check: Exp => Boolean = ( (exp) => { compare(str(exp), v_str(exp)) } )
}