import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub111 {
  
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
          case Var(a) => { if (a == var0) Const(1) else Const(0) }
          case Power(a, n) => {
            n match {
              case 0 => { Const(0) }
              case 1 => { if (a == var0) Const(1) else Const(0) }
              case 2 => {
                if (a == var0) Times(List(Const(2), Var(a))) else Const(0)
              }
              case _ => {
                
                  if (
                    a == var0
                  ) {
                    Times(List(Const(n), Power(a, n - 1))) 
                  } else {
                    Const(0)
                  }
              }
            }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, var0) }
                  case Cons(thd, Nil()) => {
                    Sum(
                      List(Times(List(diff(hd, var0), thd)),
                       Times(List(hd, diff(thd, var0)))))
                  }
                  case _ => {
                    Sum(
                      List(Times(List(diff(hd, var0), Times(tl))),
                       Times(List(hd, diff(Times(tl), var0)))))
                  }
                }
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, var0) }
                  case Cons(thd, Nil()) => {
                    Sum(List(diff(hd, var0), diff(thd, var0)))
                  }
                  case _ => { Sum(List(diff(hd, var0), diff(Sum(tl), var0))) }
                }
              }
            }
          }
        }
    }
  }
}