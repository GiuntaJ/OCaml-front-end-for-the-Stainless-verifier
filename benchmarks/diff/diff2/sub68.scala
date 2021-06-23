import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub68 {
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
          case Const(c) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Const(0) }
          case Power(v, n) => {
            if (v ne x) Const(0) else Times(List(Const(n), Power(v, n - 1)))
          }
          case Times(Cons(th, tt)) => {
            Sum(
              List(Times(diff(th, x) :: tt),
               Times(List(th, diff(Times(tt), x)))))
          }
          case Sum(s) => { Sum(s.map(( (aexp2) => { diff(aexp2, x) } ))) }
          case Times(Nil()) => { Const(0) }
        }
    }
  }
}