import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub77 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(x) => { Const(0) }
          case Var(x) => { if (var0 == x) Const(1) else Const(0) }
          case Power(x, y) => {
            if (x != var0) Const(0) else Times(List(Const(y), Power(x, y - 1)))
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(diff(hd, var0) :: tl),
               Times(List(hd, diff(Times(tl), var0)))))
          }
          case Sum(x) => { Sum(x.map(( (ae) => { diff(ae, var0) } ))) }
        }
    }
  }
}