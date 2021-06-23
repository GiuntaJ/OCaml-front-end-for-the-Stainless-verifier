import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub65 {
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
          case Var(x) => { Const(1) }
          case Power(x, n) => { Times(List(Const(n), Power(x, n - 1))) }
          case Times(Cons(a, Cons(b, Nil()))) => { Times(List(a, diff(b, x))) }
          case Sum(l) => {
            l match {
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
              case Nil() => { Const(0) }
            }
          }
          case _ => { Const(0) }
        }
    }
  }
}