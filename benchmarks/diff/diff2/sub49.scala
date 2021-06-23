import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub49 {
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
          case Const(_) => { Const(0) }
          case Var(a) => { if (a != x) Const(0) else Const(1) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(x, b - 1))) else Const(0)
          }
          case Times(Cons(head, tail)) => {
            Sum(
              List(Times(diff(head, x) :: tail),
               Times(List(head, diff(Times(tail), x)))))
          }
          case Times(Nil()) => { Const(0) }
          case Sum(Cons(head, tail)) => {
            Sum(List(diff(head, x), diff(Sum(tail), x)))
          }
          case Sum(Nil()) => { Const(0) }
        }
    }
  }
}