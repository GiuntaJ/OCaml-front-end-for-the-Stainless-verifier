import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub9 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(y, n) => {
            
              if (
                y == x
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
                    Times(List(Const(n), Power(y, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                hd match {
                  case Const(n) => { Times(List(Const(n), diff(Times(tl), x))) }
                  case _ => {
                    Sum(
                      List(Times(List(diff(hd, x), Times(tl))),
                       Times(List(hd, diff(Times(tl), x)))))
                  }
                }
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}