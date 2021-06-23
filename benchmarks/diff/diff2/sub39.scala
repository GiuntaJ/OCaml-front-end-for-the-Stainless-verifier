import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub39 {
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
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x), mul(tl))),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Power(v, n) => { Times(List(Const(n), Power(v, n - 1))) }
          case Const(n) => { Const(0) }
          case Var(y) => { if (Var(y) == Var(x)) Const(1) else Var(y) }
        }
    }
  }
  def mul: List[Aexp] => Aexp = ( (lst) => { if (lst == Nil()) Const(1) else Times(lst) } )
}