import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub123 {
  
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
          case Const(a) => { Const(0) }
          case Var(s) => { if (s == var0) Const(1) else Const(0) }
          case Power(s, n) => {
            if (s == var0) Times(List(Const(n), Power(s, n - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(1) }
              case Cons(h, Nil()) => { diff(h, var0) }
              case Cons(h, t) => {
                h match {
                  case Const(0) => { Const(0) }
                  case Const(1) => { diff(Times(t), var0) }
                  case Const(n) => { Times(List(Const(n), diff(Times(t), var0)))
                  }
                  case _ => {
                    Sum(
                      List(Times(diff(h, var0) :: t),
                       Times(List(h, diff(Times(t), var0)))))
                  }
                }
              }
            }
          }
          case Sum(lst2) => {
            lst2 match {
              case Nil() => { Const(0) }
              case Cons(h, Nil()) => { diff(h, var0) }
              case Cons(h, t) => { Sum(List(diff(h, var0), diff(Sum(t), var0)))
              }
            }
          }
        }
    }
  }
}