import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub67 {
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
          case Var(y) => { if (x == y) Const(1) else Const(0) }
          case Power(y, n) => {
            if (x == y) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case Times(Cons(h, t)) => {
            Sum(List(Times(diff(h, x) :: t), Times(List(h, diff(Times(t), x)))))
          }
          case Times(Nil()) => { Const(0) }
          case Sum(lsts) => { Sum(lsts.map(( (z) => { diff(z, x) } ))) }
        }
    }
  }
}