import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub218 {
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
          case Const(n) => { Const(0) }
          case Var(n) => { if (n == x) Const(1) else Const(0) }
          case Power(y, n) => {
            if (y == x) Times(List(Const(n), Power(y, n - 1))) else Const(0)
          }
          case Times(lis1) => {
            lis1 match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x)) ++ tl),
                   Times(List(hd, diff(Times(tl), x)))))
              }
            }
          }
          case Sum(lis2) => {
            lis2 match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
        }
    }
  }
}