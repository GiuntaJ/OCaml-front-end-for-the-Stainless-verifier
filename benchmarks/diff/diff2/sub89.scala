import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub89 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
    def processtimelist: (Aexp, String) => Aexp = {
    case (l, var0) =>
      {
        l match {
          case Const(i) => { Const(i) }
          case Var(str) => { if (str == var0) Const(0) else Var(str) }
          case Power(str, i) => {
            
              if (
                str == var0
              ) {
                i match {
                  case 2 => { Times(List(Const(2), Var(str))) }
                  case 1 => { Const(1) }
                  case 0 => { Const(0) }
                  case _ => { Times(List(Const(i), Power(str, i - 1))) }
                } 
              } else {
                Power(str, i)
              }
          }
          case Times(Cons(hd, tl)) => {
            
              if (
                findlist(hd :: tl, var0) > 0
              ) {
                Sum(
                  List(Times(processtimelist(hd, var0) :: tl)) ++
                  List(Times(
                     List(hd) ++ List(processtimelist(Times(tl), var0))))) 
              } else {
                Const(1)
              }
          }
          case _ => { makediff(l, var0) }
        }
    }
  }
  def processsumlist: (List[Aexp], String) => List[Aexp] = {
    case (l, var0) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            hd match {
              case Var(str) => {
                
                  if (
                    str == var0
                  ) {
                    Const(1) :: processsumlist(tl, var0) 
                  } else {
                    Const(0) :: processsumlist(tl, var0)
                  }
              }
              case _ => { makediff(hd, var0) :: processsumlist(tl, var0) }
            }
          }
        }
    }
  }
  def findlist: (List[Aexp], String) => Int63 = {
    case (l, str) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            hd match {
              case Const(i) => { findlist(tl, str) }
              case Var(str2) => {
                if (str == str2) 1 + findlist(tl, str) else findlist(tl, str)
              }
              case Power(str2, i) => {
                if (str == str2) i + findlist(tl, str) else findlist(tl, str)
              }
              case Times(l) => { findlist(l, str) + findlist(tl, str) }
              case Sum(l) => { findlist(l, str) + findlist(tl, str) }
            }
          }
        }
    }
  }
  def makediff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(i) => { Const(0) }
          case Var(str) => { if (str == var0) Const(1) else Const(0) }
          case Power(str, i) => {
            
              if (
                str == var0
              ) {
                i match {
                  case 2 => { Times(List(Const(2), Var(str))) }
                  case 1 => { Const(1) }
                  case 0 => { Const(0) }
                  case _ => { Times(List(Const(i), Power(str, i - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(Cons(hd, tl)) => {
            
              if (
                findlist(hd :: tl, var0) > 0
              ) {
                Sum(
                  List(Times(processtimelist(hd, var0) :: tl)) ++
                  List(Times(
                     List(hd) ++ List(processtimelist(Times(tl), var0))))) 
              } else {
                Const(1)
              }
          }
          case Times(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => {
            
              if (
                findlist(hd :: tl, var0) > 0
              ) {
                Sum(List(makediff(hd, var0)) ++ List(makediff(Sum(tl), var0))) 
              } else {
                Const(1)
              }
          }
          case Sum(Nil()) => { Const(1) }
        }
    }
  }
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) => { makediff(exp, var0) }
  }
    			
}