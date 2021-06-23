import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub61 {
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
          case Const(y) => { Const(0) }
          case Var(t) => { if (t == x) Const(1) else Const(0) }
          case Power(s, i) => { Times(List(Const(i), Power(s, i - 1))) }
          case Times(k) => {
            k match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(
                  List(Times(List(diff(hd, x)) ++ tl)) ++
                  List(Times(List(hd) ++ List(diff(Times(tl), x)))))
              }
            }
          }
          case Sum(k) => {
            k match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                Sum(List(diff(h, x)) ++ List(Sum(List(diff(Sum(t), x)))))
              }
            }
          }
        }
    }
  }
}