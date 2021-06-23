import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub129 {
  
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
          case Const(n) => { Const(0) }
          case Var(v) => { if (v == var0) Const(1) else Const(0) }
          case Power(x, n) => {
            
              if (
                x == var0
              ) {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else if (
                    n == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(n), Power(x, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                hd match {
                  case Const(1) => { diff(Times(tl), var0) }
                  case Const(n) => { Times(List(hd, diff(Times(tl), var0))) }
                  case _ => {
                    Sum(
                      List(Times(diff(hd, var0) :: tl),
                       Times(List(hd, diff(Times(tl), var0)))))
                  }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, var0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
            }
          }
        }
    }
  }
}