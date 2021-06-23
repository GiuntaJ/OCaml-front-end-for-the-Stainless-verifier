import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub105 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def isinthelist[A](l: List[A], a: A): Boolean = {
    val _2 = {
      val temp = l.filter({
        case (x, y) => { if (x != y) false else true }
      }(
        a))
      temp match {
        case Nil() => { false }
        case Cons(hd, tl) => { true }
      }
    }
  }
  
  def isnotinthelist[A](l: List[A], a: A): Boolean = {
    val _5 = {
      val temp = l.filter({
        case (x, y) => { if (x != y) false else true }
      }(
        a))
      temp match {
        case Nil() => { true }
        case Cons(hd, tl) => { false }
      }
    }
  }
  
  
  def pvar(e: Exp): List[Var] = {
    e match {
      case P(s0, e0) => { s0 :: pvar(e0) }
      case C(e0, e1) => { pvar(e0) ++ pvar(e1) }
      case V(s0) => { Nil() }
    }
  }
  
  def vvar(e: Exp): List[Var] = {
    e match {
      case P(s0, e0) => { vvar(e0) }
      case C(e0, e1) => { vvar(e0) ++ vvar(e1) }
      case V(s0) => { List(s0) }
    }
  }
  
  def check(e: Exp): Boolean = {
    val _8 = {
      val l0 = pvar(e)
      val _9 = {
        val l1 = vvar(e)
        l1.filter(isnotinthelist(l0)) match {
          case Nil() => { true }
          case Cons(hd, tl) => { false }
        }
      }
    }
  }
}