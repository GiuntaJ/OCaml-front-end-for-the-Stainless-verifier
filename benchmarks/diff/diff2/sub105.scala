import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub105 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def map(f, ((l, var0))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => { f(hd, var0) :: map(f, tl, var0) }
    }
  }
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(x) => { if (x != var0) Const(0) else Const(1) }
          case Power(x, n) => {
            if (x != var0) Const(0) else Times(List(Const(n), Power(x, n - 1)))
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(diff(hd, var0) :: tl),
                   Times(List(hd, diff(Times(tl), var0)))))
              }
            }
          }
          case Sum(lst) => { Sum(map(diff, lst, var0)) }
        }
    }
  }
}