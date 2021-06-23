import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub180 {
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
          case Const(a) => { Const(0) }
          case Var(v) => { Const(1) }
          case Power(v, a) => {
            if (v == x) Times(List(Const(a), Power(v, a - 1))) else Const(0)
          }
          case Times(l) => {
            val _5 = {
              def iter: (List[Aexp], List[Aexp], List[Aexp]) => List[Aexp] = {
                case (l1, l2, sum) =>
                  {
                    l1 match {
                      case Nil() => { sum }
                      case Cons(hd, tl) => {
                        iter(
                          tl, hd :: l2, Times(diff(hd, x) :: tl ++ l2) :: sum)
                      }
                    }
                }
              }
              Sum(iter(l, Nil(), Nil()))
            }
          }
          case Sum(l) => {
            val _2 = {
              def iter(l, res) = {
                l match {
                  case Nil() => { res }
                  case Cons(hd, tl) => { iter(tl, diff(hd, x) :: res) }
                }
              }
              Sum(iter(l, Nil()))
            }
          }
        }
    }
  }
}