import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub279 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  def diffTimesList(l: List[Aexp], x: String): List[Aexp] = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case Const(c) => {
            if (c == 0) List(Const(0)) else hd :: diffTimesList(tl, x)
          }
          case Var(v) => { if (v == x) diffTimesList(tl, x) else List(Const(0))
          }
          case Power(v, p) => {
            
              if (
                v == x
              ) {
                List(Const(p), Power(v, p - 1)) ++ diffTimesList(tl, x) 
              } else {
                hd :: diffTimesList(tl, x)
              }
          }
          case Times(l2) => { diffTimesList(l2, x) ++ diffTimesList(tl, x) }
        }
      }
    }
  }
      
  def diffSumList(l: List[Aexp], x: String): List[Aexp] = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case Const(c) => { diffSumList(tl, x) }
          case Var(v) => {
            if (v == x) Const(1) :: diffSumList(tl, x) else diffSumList(tl, x)
          }
          case Power(v, p) => {
            
              if (
                v == x
              ) {
                Times(List(Const(p), Power(v, p - 1))) :: diffSumList(tl, x) 
              } else {
                diffSumList(tl, x)
              }
          }
          case Times(l2) => { Times(diffTimesList(l2, x)) :: diffSumList(tl, x)
          }
          case Sum(l2) => { diffSumList(l2, x) ++ diffSumList(tl, x) }
        }
      }
    }
  }
        
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(c) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, p) => { Times(List(Const(p), Power(v, p - 1))) }
          case Times(l) => { Times(diffTimesList(l, x)) }
          case Sum(l) => { Sum(diffSumList(l, x)) }
        }
    }
  }
}