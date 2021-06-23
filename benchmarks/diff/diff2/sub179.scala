import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub179 {
  /* problem 4*/ 
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (e, v) =>
      {
        e match {
          case Const(a) => { Const(0) }
          case Var(s) => { if (s == v) Const(1) else Const(0) }
          case Power(s, n) => {
            if (s == v) Times(List(Const(n), Power(s, n - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(1) }
              case Cons(h, Nil()) => { diff(h, v) }
              case Cons(h, t) => {
                h match {
                  case Const(0) => { Const(0) }
                  case Const(1) => { diff(Times(t), v) }
                  case Const(n) => { Times(List(Const(n), diff(Times(t), v))) }
                  case _ => {
                    Sum(
                      List(Times(diff(h, v) :: t),
                       Times(List(h, diff(Times(t), v)))))
                  }
                }
              }
            }
          }
          case Sum(lst2) => {
            lst2 match {
              case Nil() => { Const(0) }
              case Cons(h, Nil()) => { diff(h, v) }
              case Cons(h, t) => { Sum(List(diff(h, v), diff(Sum(t), v))) }
            }
          }
        }
    }
  }
}