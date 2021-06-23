import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub161 {
  
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
          case Const(i) => { Const(0) }
          case Var(s) => { if (s == var0) Const(1) else Const(0) }
          case Power(s, i) => {
            if (s == var0) Times(List(Const(i), Power(s, i - 1))) else Const(0)
          }
          case Times(aexp_list) => {
            val _5 = {
              def times_list_differentiate(lst) = {
                lst match {
                  case Nil() => { List(Const(0)) }
                  case Cons(h, Nil()) => { List(diff(h, var0)) }
                  case Cons(h, t) => {
                    List(Times(List(diff(h, var0), Times(t))),
                     Times(List(h, diff(Times(t), var0))))
                  }
                }
              }
              Sum(times_list_differentiate(aexp_list))
            }
          }
          case Sum(aexp_list) => {
            val _2 = {
              def sum_list_differentiate(lst) = {
                lst match {
                  case Nil() => { List(Const(0)) }
                  case Cons(h, Nil()) => { List(diff(h, var0)) }
                  case Cons(h, t) => {
                    diff(h, var0) :: sum_list_differentiate(t)
                  }
                }
              }
              Sum(sum_list_differentiate(aexp_list))
            }
          }
        }
    }
  }
}