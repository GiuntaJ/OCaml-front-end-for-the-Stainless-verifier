import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub40 {
  
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
          case Const(c) => { Const(c) }
          case Var(v) => {
            v match {
              case x => { Times(List(Const(0), Var(x))) }
            }
          }
          case Power(p, a) => {
            (p, a) match {
              case (x, a) => { Times(List(Const(a), Power(x, a - 1))) }
            }
          }
          case Times(Nil()) => { Const(1) }
          case Times(Cons(hd, tl)) => {
            Times(List(diff(hd, x), diff(Times(tl), x)))
          }
          case Sum(Nil()) => { Const(0) }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
        }
    }
  }
}