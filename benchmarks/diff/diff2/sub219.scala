import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub219 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(n) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, n) => {
            
              if (
                a == x && n == 2
              ) {
                Times(List(Const(2), Var(a))) 
              } else if (
                a == x
              ) {
                Times(List(Const(n), Power(a, n - 1))) 
              } else {
                Const(0)
              }
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Failure with unaccepted form") }
              case Cons(e, Nil()) => {
                assert(false, "Failure with unaccepted form")
              }
              case Cons(e1, Cons(e2, Nil())) => {
                Sum(
                  List(Times(List(diff(e1, x), e2)),
                   Times(List(e1, diff(e2, x)))))
              }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x), Times(tl))),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(m) => {
            m match {
              case Nil() => { assert(false, "Failure with unaccepted form") }
              case Cons(e, Nil()) => {
                assert(false, "Failure with unaccepted form")
              }
              case Cons(e1, Cons(e2, Nil())) => {
                Sum(List(diff(e1, x), diff(e2, x)))
              }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}