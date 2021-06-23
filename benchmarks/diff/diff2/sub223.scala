import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub223 {
  /* problem 4 */
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def func(aexp, x) = {
            aexp match {
              case Const(_) => { Const(0) }
              case Var(y) => { if (x == y) Const(1) else Const(0) }
              case Power(y, n) => {
                if (x == y) Times(List(Const(n), Power(y, n - 1))) else Const(0)
              }
              case Times(lst) => {
                lst match {
                  case Nil() => { assert(false, "Failure with fail") }
                  case Cons(t, Nil()) => { assert(false, "Failure with fail") }
                  case Cons(t1, Cons(t2, Nil())) => {
                    Sum(
                      List(Times(List(func(t1, x), t2)),
                       Times(List(t1, func(t2, x)))))
                  }
                  case Cons(hd, tl) => {
                    Sum(
                      List(Times(List(func(hd, x), Times(tl))),
                       Times(List(hd, func(Times(tl), x)))))
                  }
                }
              }
              case Sum(lst) => {
                lst match {
                  case Nil() => { assert(false, "Failure with fail") }
                  case Cons(s, Nil()) => { assert(false, "Failure with fail") }
                  case Cons(s1, Cons(s2, Nil())) => {
                    Sum(List(func(s1, x), func(s2, x)))
                  }
                  case Cons(hd, tl) => {
                    Sum(List(func(hd, x), func(Sum(tl), x)))
                  }
                }
              }
            }
          }
          func(e, x)
        }
    }
  }
}