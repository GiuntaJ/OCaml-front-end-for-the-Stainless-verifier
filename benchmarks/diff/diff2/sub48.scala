import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub48 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  sealed case class Inval() extends Exception {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(k) => { if (k == x) Const(1) else Const(0) }
          case Power(k, n) => {
            
              if (
                k == x
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
            
              if (
                l.contains(Const(0))
              ) {
                Const(0) 
              } else {
                l match {
                  case Nil() => { assert(false, "Inval") }
                  case Cons(hd, Nil()) => { diff(hd, x) }
                  case Cons(hd, tl) => {
                    hd match {
                      case Const(1) => { diff(Times(tl), x) }
                      case Const(n) => { Times(List(hd, diff(Times(tl), x))) }
                      case _ => {
                        Sum(
                          List(Times(diff(hd, x) :: tl),
                           Times(List(hd, diff(Times(tl), x)))))
                      }
                    }
                  }
                }
              }
          }
          case Sum(l) => {
            l match {
              case Nil() => { assert(false, "Inval") }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}