import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub231 {
  sealed case class NOANSWER() extends Exception {}
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
          case Const(c) => { Const(0) }
          case Var(f) => { if (f == x) Const(1) else Const(0) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { assert(false, "NOANSWER") }
              case Cons(a, Nil()) => { Times(List(diff(a, x))) }
              case Cons(h, t) => {
                Sum(
                  List(Times(diff(h, x) :: t),
                   Times(List(h, diff(Times(t), x)))))
              }
            }
          }
          case Sum(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(Sum(t), x), diff(h, x))) }
            }
          }
        }
    }
  }
}