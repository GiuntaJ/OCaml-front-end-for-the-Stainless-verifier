import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda2_sub151 {
  sealed abstract class Exp {}
  case class V(param0: Var) extends Exp {}
  case class P(param0: Var,  param1: Exp) extends Exp {}
  case class C(param0: Exp,  param1: Exp) extends Exp {}
  
  type Var = String
  
  def helpcheck: Exp => List[Exp] = (
    (exp) =>
      {
        exp match {
          case P(a, b) => { V(a) :: helpcheck(b) }
          case C(a, b) => { helpcheck(a) ++ helpcheck(b) }
          case V(a) => { Nil() }
        }
    }
  )
  
  def varcheck: Exp => List[Exp] = (
    (exp) =>
      {
        exp match {
          case V(a) => { List(V(a)) }
          case C(a, b) => { varcheck(a) ++ varcheck(b) }
          case P(a, b) => { varcheck(b) }
        }
    }
  )
  
  def findvar: Exp => List[Exp] = (
    (exp) =>
      {
        exp match {
          case V(a) => { List(V(a)) }
          case P(a, b) => { V(a) :: findvar(b) }
          case C(a, b) => { findvar(a) ++ findvar(b) }
        }
    }
  )
  
  def helpcheck2: (Exp, Exp) => List[Exp] = {
    case (ex, exp) =>
      {
        ex match {
          case P(a, b) => {
            
              if (
                b == exp
              ) {
                V(a) :: helpcheck2(b, exp) 
              } else {
                V(a) :: helpcheck2(b, exp)
              }
          }
          case C(a, b) => {
            
              if (
                a == exp
              ) {
                Nil() 
              } else if (
                b == exp
              ) {
                helpcheck2(a, exp) 
              } else {
                helpcheck2(a, exp) ++ helpcheck2(b, exp)
              }
          }
          case V(a) => { Nil() }
        }
    }
  }
  
  def confi: (List[Exp], Exp) => Boolean = {
    case (a, b) =>
      {
        a match {
          case Cons(hd, tl) => { if (hd == b) true else confi(tl, b) }
          case _ => { false }
        }
    }
  }
  
  def last: (List[Exp], Exp) => Boolean = {
    case (a, b) =>
      {
        a match {
          case Cons(hd, tl) => { confi(helpcheck2(b, hd), hd) && last(tl, b) }
          case Nil() => { true }
        }
    }
  }
  
  def check: Exp => Boolean = ( (exp) => { if (last(varcheck(exp), exp)) true else false } )
}
