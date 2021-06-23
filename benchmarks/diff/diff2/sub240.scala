import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub240 {
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
          case Times(Cons(Const(n), Cons(Var(x), Nil()))) => { Var(x) }
          case Power(x, n) => { Times(List(Const(n), Power(x, n - 1))) }
          case Sum(Cons(hd, Cons(tl, Nil()))) => { diff(Sum(List(exp)), x) }
        }
    }
  }
}