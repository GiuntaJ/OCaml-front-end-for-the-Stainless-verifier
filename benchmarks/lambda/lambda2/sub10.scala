import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub10 {
    sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def comblist: (List[A], List[A]) => List[A] = {
    case (l1, l2) =>
      {
        l1 match {
          case Nil() => { l2 }
          case Cons(hd, tl) => { hd :: comblist(tl, l2) }
        }
    }
  }
  
  def searchlist: (String, List[String]) => Boolean = {
    case (a, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(hd, tl) => { if (hd == a) true else searchlist(a, tl) }
        }
    }
  }
  
  def complist: List[String] => List[String] = (
    (l) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            if (searchlist(hd, tl)) complist(tl) else hd :: tl
          }
        }
    }
  )
  
  def vars: Exp => List[String] = (
    (e) =>
      {
        e match {
          case V(x) => { List(x) }
          case P(x, e1) => { vars(e1) }
          case C(e1, e2) => { comblist(vars(e1), vars(e2)) }
        }
    }
  )
  
  def used: Exp => List[String] = (
    (e) =>
      {
        e match {
          case V(x) => { Nil() }
          case P(x, e1) => { x :: used(e1) }
          case C(e1, e2) => { comblist(used(e1), used(e2)) }
        }
    }
  )
  
  def matchvar: (List[String], List[String]) => Boolean = {
    case (vr, us) =>
      {
        (vr, us) match {
          case (Nil(), _) => { true }
          case (Cons(hd, tl), us) => {
            if (searchlist(hd, us)) matchvar(tl, us) else false
          }
        }
    }
  }
    
    val check: Exp => Boolean = ( (e) => { matchvar(complist(vars(e)), used(e)) } )
}