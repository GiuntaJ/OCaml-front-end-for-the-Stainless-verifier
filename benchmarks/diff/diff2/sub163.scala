import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub163 {
  
  sealed case class NotImplemented() extends Exception {}
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == var0) Const(1) else Const(0) }
          case Power(a, b) => {
            
              if (
                a != var0 || b == 0
              ) {
                Const(0) 
              } else if (
                b == 1
              ) {
                Const(1) 
              } else if (
                b == 2
              ) {
                Times(List(Const(b), Var(a))) 
              } else {
                Times(List(Const(b), Power(a, b - 1)))
              }
          }
          case Times(a) => {
            a match {
              case Nil() => { assert(false, "NotImplemented") }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                
                  if (
                    diff(hd, var0) == Const(0)
                  ) {
                    Times(List(hd) ++ List(diff(Times(tl), var0))) 
                  } else {
                    Sum(
                      List(Times(List(diff(hd, var0)) ++ tl),
                       Times(List(hd) ++ List(diff(Times(tl), var0)))))
                  }
              }
            }
          }
          case Sum(a) => {
            a match {
              case Nil() => { assert(false, "NotImplemented") }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                
                  if (
                    diff(hd, var0) == Const(0)
                  ) {
                    diff(Sum(tl), var0) 
                  } else {
                    diff(Sum(tl), var0) match {
                      case Sum(b) => { Sum(List(diff(hd, var0)) ++ b) }
                      case Const(0) => { Sum(List(diff(hd, var0))) }
                      case _ => {
                        Sum(List(diff(hd, var0)) ++ List(diff(Sum(tl), var0)))
                      }
                    }
                  }
              }
            }
          }
        }
    }
  }
}