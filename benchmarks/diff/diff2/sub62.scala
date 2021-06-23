import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub62 {
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
          case Const(i) => { Const(0) }
          case Var(a) => { if (a == "x") Const(1) else Const(0) }
          case Power(a, i) => { Times(List(Const(i), Power(a, i - 1))) }
          case Times(Cons(h, t)) => { Times(List(h, diff(Sum(t), x))) }
          case Sum(Cons(h, t)) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
          case _ => { Const(0) }
        }
    }
  }
}
