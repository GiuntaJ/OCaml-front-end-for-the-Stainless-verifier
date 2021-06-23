import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub63 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, n) => {
            if (v == x) Times(List(Const(n), Power(v, n - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                Sum(
                  List(Times(diff(h, x) :: t),
                   Times(List(h, diff(Times(t), x)))))
              }
            }
          }
          case Sum(l) => {
            val _2 = {
              def f(l, ll) = {
                l match {
                  case Nil() => { Sum(ll) }
                  case Cons(h, t) => {
                    val _5 = {
                      val ll = ll ++ List(diff(h, x))
                      f(t, ll)
                    }
                  }
                }
              }
              f(l, Nil())
            }
          }
        }
    }
  }
}