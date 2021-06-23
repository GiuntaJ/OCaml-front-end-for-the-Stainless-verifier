import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub12 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff(((aexp, str))) = {
    aexp match {
      case Const(n) => { Const(0) }
      case Var(x) => { if (x == str) Const(1) else Const(0) }
      case Power(x, n) => {
        
          if (
            n == 1
          ) {
            Const(1) 
          } else if (
            x == str
          ) {
            Times(List(Const(n), Power(x, n - 1))) 
          } else {
            Const(0)
          }
      }
      case Times(alist) => {
        alist match {
          case Nil() => { assert(false, "Failure with Empty List") }
          case Cons(hd, Nil()) => { diff(hd, str) }
          case Cons(hd, tl) => {
            hd match {
              case Const(0) => { Const(0) }
              case Const(1) => { diff(Times(tl), str) }
              case Const(n) => { Times(List(hd, diff(Times(tl), str))) }
              case Var(x) => { Times(List(diff(hd, str), diff(Times(tl), str)))
              }
              case Power(x, n) => {
                Times(List(diff(Power(x, n), str), diff(Times(tl), str)))
              }
              case Times(alist2) => {
                Sum(
                  List(Times(List(diff(Times(alist2), str), Times(tl))),
                   Times(List(Times(alist2), diff(Times(tl), str)))))
              }
              case Sum(alist2) => {
                Sum(
                  List(Times(List(diff(Sum(alist2), str), Times(tl))),
                   Times(List(Sum(alist2), diff(Times(tl), str)))))
              }
            }
          }
        }
      }
      case Sum(alist) => {
        alist match {
          case Nil() => { assert(false, "Failure with Empty List") }
          case Cons(hd, Nil()) => { diff(hd, str) }
          case Cons(hd, tl) => {
            hd match {
              case Const(0) => { Const(0) }
              case Const(n) => { diff(Sum(tl), str) }
              case Var(x) => { Sum(List(diff(Var(x), str), diff(Sum(tl), str)))
              }
              case Power(x, n) => {
                Sum(List(diff(Power(x, n), str), diff(Sum(tl), str)))
              }
              case Times(alist2) => {
                Sum(List(diff(Times(alist2), str), diff(Sum(tl), str)))
              }
              case Sum(alist2) => {
                Sum(List(diff(Sum(alist2), str), diff(Sum(tl), str)))
              }
            }
          }
        }
      }
    }
  }
}