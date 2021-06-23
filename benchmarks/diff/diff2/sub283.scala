import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub283 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(y, n) => {
            if (y == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Cons(y, Nil()) => { diff(y, x) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, x) :: tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
              case Nil() => { Const(0) }
            }
          }
          case Sum(lst) => {
            val _2 = {
              def diffsum: List[Aexp] => List[Aexp] = (
                (lst) =>
                  {
                    lst match {
                      case Cons(hd, tl) => { diff(hd, x) :: diffsum(tl) }
                      case Nil() => { List(Const(0)) }
                    }
                }
              )
              Sum(diffsum(lst))
            }
          }
        }
    }
  }
}