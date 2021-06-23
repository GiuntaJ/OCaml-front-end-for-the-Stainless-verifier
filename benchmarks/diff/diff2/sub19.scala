import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub19 {
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
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(hd, diff(Sum(tl), x))),
                   Times(diff(hd, x) :: tl)))
              }
            }
          }
          case Sum(l) => {
            val _2 = {
              def iterSum(a) = { diff(a, x) }
              Sum(l.map(iterSum))
            }
          }
        }
    }
  }
}