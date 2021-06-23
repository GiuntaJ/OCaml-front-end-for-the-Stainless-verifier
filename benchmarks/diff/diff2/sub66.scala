import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub66 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def iscontainconstzero(lst) = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => {
        if (hd == Const(0)) true else iscontainconstzero(tl)
      }
    }
  }
  
  sealed case class InvalidArgument() extends Exception {}
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(p, n) => {
            
              if (
                p == x
              ) {
                n match {
                  case 0 => { Const(0) }
                  case 1 => { Const(1) }
                  case _ => { Times(List(Const(n), Power(p, n - 1))) }
                } 
              } else {
                Const(0)
              }
          }
          case Times(t) => {
            
              if (
                iscontainconstzero(t)
              ) {
                Const(0) 
              } else {
                t match {
                  case Nil() => { assert(false, "InvalidArgument") }
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
          case Sum(s) => {
            s match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}