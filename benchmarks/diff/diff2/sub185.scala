import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub185 {
  sealed case class Problem() extends Exception {}
  
  /*problem4*/
  
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
          case Const(_) => { Const(0) }
          case Var(y) => { if (x == y) Const(1) else Const(0) }
          case Power(y, n) => {
            if (x == y) Times(List(Const(n), Power(y, n - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "Problem") }
              case Cons(a, Nil()) => { assert(false, "Problem") }
              case Cons(a1, Cons(a2, Nil())) => {
                Sum(
                  List(Times(List(diff(a1, x), a2)),
                   Times(List(a1, diff(a2, x)))))
              }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x), Times(tl))),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { assert(false, "Problem") }
              case Cons(a, Nil()) => { assert(false, "Problem") }
              case Cons(a1, Cons(a2, Nil())) => {
                Sum(List(diff(a1, x), diff(a2, x)))
              }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}